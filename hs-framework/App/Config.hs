
module App.Config where

import Data.IORef
import System
import Misc

type Config = IORef [(String,String)]

initConfig :: IO Config
initConfig = do
  cfg <- newIORef []
  fn <- configFile
  cts <- readFile fn `catch` (\_->return "[]")
  xs <- readIO cts `catch` (\_->return [])
  writeIORef cfg xs
  return cfg

readConfigVar :: Config -> String -> IO (Maybe String)
readConfigVar cfg var = do
  m <- lookup var $^ readIORef cfg
  return m

writeConfigVar cfg var val = do
  modifyIORef cfg (((var,val):) . filter ((/=var) . fst))

storeConfig cfg = do
  pn <- configFile
  cts <- readIORef cfg
  writeFile pn (show cts)


configFile = do
  pn <- getProgName
  return (takeWhile (/='.') pn++".cfg")

