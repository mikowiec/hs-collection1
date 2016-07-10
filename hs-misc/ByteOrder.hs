
module ByteOrder where

import Data.Bits
import Data.Word

import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe

swap32 :: Word32 -> Word32
swap32 w = fromIntegral (swap16 ls16) `shiftL` 16 .|. fromIntegral (swap16 ms16)
 where ls16 = fromIntegral (w .&. 0xffff)
       ms16 = fromIntegral (w `shiftR` 16)

swap16 :: Word16 -> Word16
swap16 w = lsb `shiftL` 8 .|. msb
 where lsb = w .&. 0xff
       msb = w `shiftR` 8


listToWord32BE :: [Word8] -> Word32
listToWord32BE [a,b,c,d] =
  fromIntegral a `shiftL` 24 +
  fromIntegral b `shiftL` 16 +
  fromIntegral c `shiftL`  8 +
  fromIntegral d

word32ToListBE :: Word32 -> [Word8]
word32ToListBE w = [a,b,c,d]
 where a = fromIntegral $ (w `shiftR` 24) .&. 0xff
       b = fromIntegral $ (w `shiftR` 16) .&. 0xff
       c = fromIntegral $ (w `shiftR`  8) .&. 0xff
       d = fromIntegral $ (w `shiftR`  0) .&. 0xff

listToWord32LE :: [Word8] -> Word32
listToWord32LE [d,c,b,a] =
  fromIntegral a `shiftL` 24 +
  fromIntegral b `shiftL` 16 +
  fromIntegral c `shiftL`  8 +
  fromIntegral d

word32ToListLE :: Word32 -> [Word8]
word32ToListLE w = [d,c,b,a]
 where a = fromIntegral $ (w `shiftR` 24) .&. 0xff
       b = fromIntegral $ (w `shiftR` 16) .&. 0xff
       c = fromIntegral $ (w `shiftR`  8) .&. 0xff
       d = fromIntegral $ (w `shiftR`  0) .&. 0xff


{-# NOINLINE isLittleEndian #-}
isLittleEndian :: Bool
isLittleEndian = unsafePerformIO $ do
  allocaBytes 4 $ \ptr -> do
    poke ptr (0x1234ABCD :: Word32)
    lsb <- peekByteOff ptr 0 :: IO Word8
    return (lsb == 0xCD)

networkOrderSwap :: Word32 -> Word32
networkOrderSwap = if isLittleEndian then swap32 else id


