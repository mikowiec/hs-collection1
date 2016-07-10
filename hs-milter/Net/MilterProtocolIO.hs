
module Net.MilterProtocolIO where


import Net.MilterProtocol

import ByteOrder

import Data.Bits
import Data.Word
import Data.Int
import Data.Array.IO
import Data.Array.Unboxed
import Data.Array.MArray
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO

import Text.Printf
import Misc

import Monad
import Char


readPacket :: Handle -> IO Packet
readPacket h = do
    allocaBytes 4 $ \p -> do
        4 <- hGetBuf h p 4
        len <- fromIntegral . swap32 $^ peek p
        when (len == 0) $ fail "null packet"
        1 <- hGetBuf h p 1
        cmd <- peek (castPtr p) :: IO CChar
        arr <- newArray_ (0,len - 2)
        n <- hGetArray h arr (len-1)
        when (n /= len-1) $ fail ("end of stream? (read " ++ show n ++ " of " ++ show len ++ ")")
        arr' <- freeze arr
        return $ Packet cmd arr'


writePacket :: Handle -> Packet -> IO ()
writePacket h (Packet cmd arr) = do
    let b = bounds arr
    let len = snd b - fst b + 1
    allocaBytes 5 $ \p -> do
        poke p (swap32 (fromIntegral (len+1)))
        pokeByteOff (castPtr p) 4 cmd
        hPutBuf h p 5
        arr' <- thaw arr
        hPutArray h arr' len
        return ()

zstrings :: UArray Int Word8 -> [String]
zstrings arr = zstringsOfList (elems arr)


zstringsOfList = f
 where f :: [Word8] -> [String]
       f [] = []
       f xs = case span (/=0) xs of
               (v,0:ys) -> ret v : f ys
               (v,[]) -> [ret v]
       ret = map (chr . fromIntegral)


takeOdd [] = []
takeOdd (x:xs) = takeEven xs
takeEven [] = []
takeEven (x:xs) = x : takeOdd xs

zipPairs [] = []
zipPairs (x:y:zs) = (x,y) : zipPairs zs

packetToCommand :: Packet -> Command
packetToCommand (Packet cmd arr) = 
  case fromByte (fromIntegral cmd) of
   SMFIC_ABORT   -> SMFIC_Abort
   SMFIC_BODY    -> SMFIC_Body arr
   SMFIC_CONNECT -> let [host,family_char:port_high:port_low:address] = zstrings arr
                        family = fromByte (fromIntegral (ord family_char))
                        port = (fromIntegral (ord port_high)) `shiftL` 8 + fromIntegral (ord port_low)
                    in SMFIC_Connect host family port address
   SMFIC_MACRO   -> case elems arr of
                     (c:zs) -> SMFIC_Macro (fromByte (fromIntegral (c)))
                                           (zipPairs (zstringsOfList zs))
   SMFIC_BODYEOB -> SMFIC_BodyEob
   SMFIC_HELO    -> SMFIC_Helo (arr2zstr arr)
   SMFIC_HEADER  -> let [s1,s2] = zstrings arr in SMFIC_Header s1 s2
   SMFIC_MAIL    -> let (s:ss) = zstrings arr in SMFIC_Mail s ss
   SMFIC_EOH     -> SMFIC_Eoh
   SMFIC_OPTNEG  -> let [v1,v2,v3,v4,0,0,0,a,0,0,0,p] = elems arr
                    in SMFIC_OptNeg (beList2word [v1,v2,v3,v4])
                                    [s | (s,v) <- byteReps, a .&. v /= 0]
                                    [s | (s,v) <- byteReps, p .&. v /= 0]
   SMFIC_RCPT    -> let (s:ss) = zstrings arr in SMFIC_Rcpt s ss
   SMFIC_QUIT    -> SMFIC_Quit
   
toCC = fromIntegral . toByte

nil = array (0,-1) []

arr2str :: UArray Int Word8 -> String
arr2str = map (chr . fromIntegral) . elems

arr2zstr :: UArray Int Word8 -> String
arr2zstr = takeWhile (/='\0') . map (chr . fromIntegral) . elems

str2arr :: String -> UArray Int Word8
str2arr s = 
 let len = length s
 in listArray (0,len-1) (map (fromIntegral . ord) s)

zstr2arr :: String -> UArray Int Word8
zstr2arr s = 
 let len = length s
 in listArray (0,len) (map (fromIntegral . ord) s ++ [0])

word2beList :: Word32 -> [Word8]
word2beList i = map fromIntegral
  [(i `shiftR` 24) .&. 0xff,
   (i `shiftR` 16) .&. 0xff,
   (i `shiftR`  8) .&. 0xff,
   (i `shiftR`  0) .&. 0xff]

beList2word :: [Word8] -> Word32
beList2word [i0,i1,i2,i3] =
  (fromIntegral i0 `shiftL` 24) +
  (fromIntegral i1 `shiftL` 16) +
  (fromIntegral i2 `shiftL` 8) +
  fromIntegral i3

responseToPacket :: Response -> Packet
responseToPacket r =
  case r of
   SMFIR_AddRcpt rcpt  -> Packet (toCC SMFIR_ADDRCPT) (zstr2arr rcpt)
   SMFIR_DelRcpt rcpt  -> Packet (toCC SMFIR_DELRCPT) (zstr2arr rcpt)
   SMFIR_Accept        -> Packet (toCC SMFIR_ACCEPT) nil
   SMFIR_Continue      -> Packet (toCC SMFIR_CONTINUE) nil
   SMFIR_Discard       -> Packet (toCC SMFIR_DISCARD) nil
   SMFIR_Progress      -> Packet (toCC SMFIR_PROGRESS) nil
   SMFIR_Reject        -> Packet (toCC SMFIR_REJECT) nil
   SMFIR_TempFail      -> Packet (toCC SMFIR_TEMPFAIL) nil
   SMFIR_ReplBody body -> Packet (toCC SMFIR_REPLBODY) body
   SMFIR_Quarantine q  -> Packet (toCC SMFIR_QUARANTINE) (zstr2arr q)
   SMFIR_ReplyCode code desc   -> Packet (toCC SMFIR_REPLYCODE) (zstr2arr (show code ++ " " ++ desc))
   SMFIR_AddHeader label value -> Packet (toCC SMFIR_ADDHEADER) (str2arr (label ++ "\0" ++ value ++ "\0"))
   SMFIR_ChgHeader index label value ->
    Packet (toCC SMFIR_CHGHEADER) (str2arr ("    " ++ label ++ "\0" ++ value ++ "\0") // zip [0..] (word2beList index))
   SMFIR_OptNeg version actionMask protocolMask ->
    Packet (toCC SMFIR_OPTNEG)
      (listArray (0,11)
        (word2beList version ++
         [0,0,0,sum (map toByte actionMask)] ++
         [0,0,0,sum (map toByte protocolMask)]))




