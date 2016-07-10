
module App.IDGen where

import Data.IORef
import System.IO.Unsafe
import Data.Typeable
import Control.Monad.State
import Control.Monad.Trans

type ID = Int

{-# NOINLINE id_ref #-}
id_ref :: IORef ID
id_ref = unsafePerformIO $ newIORef 0

mkId :: IO ID
mkId = atomicModifyIORef id_ref (\id -> (succ id, id))

mkIdStream :: IO [ID]
mkIdStream = do
  i <- mkId
  is <- unsafeInterleaveIO mkIdStream
  return (i:is)

mkStream :: IO IDStream
mkStream = mkIdStream >>= newIORef >>= return . IDStream

withIdStream ref f = do
  is <- readIORef ref
  (r,is') <- f is
  writeIORef ref is'
  return r

data IDStream = IDStream (IORef [ID])

genId :: (MonadState IDStream m, MonadIO m) => m ID
genId = do
  IDStream ref <- get
  liftIO $ atomicModifyIORef ref (\(i:is) -> (is,i))

--runId :: Monad m => IDStream -> StateT IDStream m a -> m a
runId ref m = do
  r <- evalStateT m ref
  return r
  

