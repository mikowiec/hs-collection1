
module Profiling where

import Time
import Foreign
import Monad
import CTypes



 -- Fallback implementation

profile_ :: IO a -> IO Float
profile_ m = do
    t1 <- getClockTime
    m
    t2 <- getClockTime
    return (tdiff_to_msec (diffClockTimes t2 t1))


tdiff_to_msec t =
     let overflow = tdYear t + tdMonth t + tdDay t in
     if overflow > 0 then error ("Too big timediff: " ++ show overflow)
      else fromIntegral (tdHour t * 3600000 + tdMin t * 60000 +  tdSec t * 1000) +
           (fromIntegral (tdPicosec t) / 1000000)

{-
profile_ :: IO a -> IO Float
profile_ m = do
    t1 <- get_time
    m
    t2 <- get_time
    return $ diff_in_msec t2 t1

diff_in_msec (t2s,t2u) (t1s,t1u) =
    fromIntegral (t2s - t1s) * 1000
     + fromIntegral (t2u - t1u) / 1000
    
get_time :: IO (Integer,Integer)
get_time = do
    p <- mallocBytes 8
    r <- gettimeofday p nullPtr
    when (r /= 0) (fail ("gettimeofday error: " ++ show r))
    sec <- peekElemOff p 0
    usec <- peekElemOff p 1
    free p
    return (fromIntegral sec, fromIntegral usec)

foreign import ccall gettimeofday :: Ptr CULong -> Ptr () -> IO Int

-}
