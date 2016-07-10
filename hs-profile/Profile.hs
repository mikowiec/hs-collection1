
module Profile where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Monad
import Text.Printf

import System.IO.Unsafe

#ifdef _WIN32
import Win32.HighPrecisionTime
#else
import Posix.HighPrecisionTime
#endif

profile m = do
    ref <- return $! recent_epoch
    b <- getTimeOfDay
    r <- m
    e <- getTimeOfDay
    return (e-b,r)

profile_report l m = do
    ref <- return $! recent_epoch
    b <- getTimeOfDay
    (idt,r) <- profile m
    let dt = fromIntegral idt / 1000 :: Float
    printf "%s: %8.3fms (%5d)s\n" l dt ((b - ref) `div` 1000000)
    return r

stamp :: String -> Int -> String -> IO Integer
stamp l ix extra = do
    ref <- return $! recent_epoch
    t <- getTimeOfDay
    printf "%20s %04d: %9.3fms %s\n" l ix (fromIntegral (t-ref) / 1000 :: Float) extra :: IO ()
    return t

ref_epoch = recent_epoch
getTime = getTimeOfDay

