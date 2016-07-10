
module Utils where

import Profile
import Data.IORef

diffCounter = do
  now <- getTime
  frame_time <- newIORef now
  return $ do
    now <- getTime
    last <- atomicModifyIORef frame_time (\last -> (now,last))
    return (now-last)

