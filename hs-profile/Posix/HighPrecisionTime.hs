{-# OPTIONS -fffi #-}

module Posix.HighPrecisionTime where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Monad

import System.IO.Unsafe

{-# NOINLINE recent_epoch #-}
recent_epoch :: Integer
recent_epoch = unsafePerformIO $ getTimeOfDay


getTimeOfDay :: IO Integer
getTimeOfDay = do
    allocaBytes 100 $ \p -> do
        r <- gettimeofday p nullPtr
        when (r /= 0) $ 
          fail "gettimeofday failure"
        sec <- peek p
        usec <- peekElemOff p 1
        return (fromIntegral sec * 1000000 + fromIntegral usec)


foreign import ccall "sys/time.h gettimeofday" gettimeofday :: Ptr CLong -> Ptr b -> IO Int


