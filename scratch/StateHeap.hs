
module StateHeap where

import IDGen
import qualified Data.Map as Map
import Data.Dynamic
import Data.IORef
import Text.Printf

nil2nothing [] = Nothing
nil2nothing xs = Just xs

type ID = Int
type Heap = IORef (Map.Map (String,ID) [Dynamic])


initHeap :: IO Heap
initHeap = newIORef (Map.empty)


read :: Typeable a => Heap -> ID -> String -> IO a
read heap id name = 
  map <- readIORef heap
  Just v <- return $ Map.lookup (id,name) map

write :: Typeable a => Heap -> ID -> String -> a -> IO ()
write heap id name val = 
  modifyIORef heap (Map.insert (name,id) val)


