{-# OPTIONS -fffi #-}
module Win32.HighPrecisionTime where

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

foreign import stdcall "windows.h QueryPerformanceFrequency" queryPerformanceFrequency :: Ptr Int64 -> IO ()
foreign import stdcall "windows.h QueryPerformanceCounter" queryPerformanceCounter :: Ptr Int64 -> IO ()

{-# NOINLINE recent_epoch #-}
recent_epoch = unsafePerformIO $ getTimeOfDay

getFrequency :: IO Integer
getFrequency = do
  alloca $ \p -> do
    queryPerformanceFrequency p
    f <- peek p
    return (fromIntegral f)

{-# NOINLINE frequency #-}
frequency :: Integer
frequency = unsafePerformIO getFrequency

getCounter :: IO Integer
getCounter = do
  alloca $ \p -> do
    queryPerformanceCounter p
    t <- peek p
    return (fromIntegral t)

getTimeOfDay :: IO Integer
getTimeOfDay = do
  t <- getCounter
  return (fromIntegral (1000000 * t) `div` fromIntegral frequency)

