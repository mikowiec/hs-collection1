
module App.Signals where

import App.IDGen
import qualified Data.Map as Map
import Data.Dynamic
import Data.IORef
import Text.Printf
import List

type Bus = ([String], IORef (Map.Map String [(ID,Dynamic)]))

initBus :: IO Bus
initBus = do
  m <- newIORef (Map.empty)
  return ([], m)

monitor :: (Typeable a) => Bus -> String -> a -> IO (IO ())
monitor (ps,bus) raw_name f = do
  let d = toDyn f
      name = slotName ps raw_name
  case typeRepArgs (typeOf f) of
   [_argsType, res] | res == typeOf (undefined :: IO ()) -> return ()
   _ -> fail (printf "Invalid type for signal handler %s :: %s" raw_name (show (typeOf f)))

  id <- mkId
  modifyIORef bus (Map.insertWith (\a b -> a++b) name [(id,d)])
  let f xs = nil2nothing (filter ((/=id) . fst) xs)
  return $ modifyIORef bus (Map.update f name)
 
 
signal :: (Show a, Typeable a) => Bus -> String -> a -> IO ()
signal (ps,bus) raw_name val = do
  let name = slotName ps raw_name
  map <- readIORef bus
  case Map.lookup name map of
   Just ds -> do
    mapM_ (\(_,d) -> do
      let f = fromDynamic d :: Typeable b => Maybe (b -> IO ())
      case f of
       Nothing -> printf "type error on %s: %s cannot be applied to %s\n" name (show d) (show (typeOf val))
       Just f' -> f' val) ds
   Nothing -> printf "No listeners for %s: %s\n" name (show map)

prefix :: Bus -> String -> Bus
prefix (ps,ref) pfx = (pfx:ps,ref)

slotName :: [String] -> String -> String
slotName ps raw_name = concat $ intersperse "-" (ps++[raw_name])

take_some :: String -> String
take_some s | length s < 30 = take 30 s
take_some s = take 27 s ++ "..."

nil2nothing :: [a] -> Maybe [a]
nil2nothing [] = Nothing
nil2nothing xs = Just xs


