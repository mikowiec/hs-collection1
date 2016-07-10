module Main ( main ) where

import System.IO
import Foreign

bufsize :: Int                  -- our I/O buffer size
bufsize = 4096

type Count = Int32
data CountingState = ST !Bool !Count !Count !Count
                     deriving (Show)

initCST :: CountingState
initCST = ST True 0 0 0

wc :: Char -> CountingState -> CountingState
wc '\n' (ST _     l w c) = (ST True (l+1)  w   (c+1))
wc ' '  (ST _     l w c) = (ST True   l    w   (c+1))
wc '\t' (ST _     l w c) = (ST True   l    w   (c+1))
wc  _   (ST True  l w c) = (ST False  l  (w+1) (c+1))
wc  _   (ST False l w c) = (ST False  l    w   (c+1))

countBuf :: Ptr Word8 -> Int -> CountingState -> IO CountingState
countBuf _   0 st@(ST _ _ _ _) = return st
countBuf ptr n st@(ST _ _ _ _) = do
  c <- fmap (toEnum . fromEnum) (peek ptr)
  countBuf (ptr `plusPtr` 1) (n - 1) (wc c st)

loop :: Handle -> Ptr Word8 -> CountingState -> IO CountingState
loop h ptr st@(ST _ _ _ _) = do
  rc <- hGetBuf h ptr bufsize
  if rc == 0
     then return st
     else countBuf ptr rc st >>= (loop h ptr $!)

main :: IO ()
main = do
  allocaArray bufsize $ \ptr -> do
    ST _ l w c <- loop stdin ptr initCST
    putStrLn . shows l . (' ':) . shows w . (' ':) . shows c $ ""

