
module Main where

import Misc
import Misc.SP
import System.IO
import Foreign

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

wc_sp :: SP IO Word8 (Count,Count,Count)
wc_sp = wc_sp' initCST

wc_sp' :: CountingState -> SP IO (Word8) (Count,Count,Count)
wc_sp' st@(ST _ l w c) = GetSP $ \ch -> case ch of
    Nothing -> PutSP (l,w,c) NullSP
    Just ch' -> wc_sp' (wc (toEnum (fromEnum ch')) st)

putN :: Ptr Word8 -> Int -> SP IO () Word8 -> SP IO () Word8
putN _ 0 sp = sp
putN ptr n sp = ActionSP $ do
    c <- peek ptr :: IO Word8
    return $ PutSP c (putN (ptr `plusPtr` 1) (n-1) sp)

feed :: Ptr Word8 -> Handle -> SP IO () Word8
feed ptr h = ActionSP $ do
    rc <- hGetBuf h ptr 4096
    case rc of
     0 -> return NullSP
     n -> return $ putN ptr n (feed ptr h)

main = do
    allocaArray 4096 $ \ptr -> do
      r <- runSP (wc_sp <-< feed ptr stdin) []
      print r
    
