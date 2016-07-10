
module NodeDB where

import Data.Map
import Data.IORef
import System.IO.Unsafe

{-# NOINLINE node_map_ref #-}
node_map_ref :: IORef (Map String [String])
node_map_ref = unsafePerformIO (newIORef empty)


registerNode :: String -> [String] -> IO ()
registerNode n as = do
  modifyIORef node_map_ref (\m -> insert n as m)


nodeMap :: IO (Map String [String])
nodeMap = readIORef node_map_ref


