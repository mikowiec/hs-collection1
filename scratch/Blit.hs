
module Blit (blit_argb8888_to_rgb888, blit_rgb565_to_rgb888) where

import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word
import Data.Bits
import Profile
import Monad
import Numeric

blit_argb8888_to_rgb888 
  :: Ptr Word8 -> Ptr Word32 -> Word32 -> Word32 -> IO ()
blit_argb8888_to_rgb888 dst src w h = do
  blit_bytes dst src (fromIntegral (w*h))

blit_bytes :: Ptr Word8 -> Ptr Word32 -> Int -> IO ()
blit_bytes _ _ 0 = return ()
blit_bytes dst src n = do
  v <- peek src :: IO Word32
  poke dst (fromIntegral ((v `shiftR` 16) .&. 0xff) :: Word8)
  poke (dst `plusPtr` 1) (fromIntegral ((v `shiftR` 8) .&. 0xff) :: Word8)
  poke (dst `plusPtr` 2) (fromIntegral (v .&. 0xff) :: Word8)
  blit_bytes (dst `plusPtr` 3) (src `plusPtr` 4) (n-1)



blit_rgb565_to_rgb888 
  :: Ptr Word8 -> Ptr Word16 -> Word32 -> Word32 -> IO ()
blit_rgb565_to_rgb888 dst src w h = do
  blit_bytes_565 dst src (fromIntegral (w*h))

blit_bytes_565 :: Ptr Word8 -> Ptr Word16 -> Int -> IO ()
blit_bytes_565 _ _ 0 = return ()
blit_bytes_565 dst src n = do
  v <- peek src :: IO Word16
  let rs = v .&. 0xf800
      gs = v .&. 0x07e0
      bs = v .&. 0x001f
  let r = (rs `shiftR` 8)
      g = (gs `shiftR` 3)
      b = (bs `shiftL` 3)
  let r' = r .|. ((r `shiftR` 3) .&. 0x7)
      g' = g .|. ((g `shiftR` 2) .&. 0x3)
      b' = b .|. ((b `shiftR` 3) .&. 0x7)
  poke dst               ((fromIntegral r') :: Word8)
  poke (dst `plusPtr` 1) ((fromIntegral g') :: Word8)
  poke (dst `plusPtr` 2) ((fromIntegral b') :: Word8)
  blit_bytes_565 (dst `plusPtr` 3) (src `plusPtr` 2) (n-1)

hx v = showHex v ""

main :: IO ()
main = do
  let w = 500
      h = 500
  allocaBytes (w*h*4) $ \src ->
    allocaBytes (w*h*3) $ \dst -> do
     let _ = src :: Ptr Word32
         _ = dst :: Ptr Word8
     profile_report "blit" $
      sequence_ (replicate 1 (blit_argb8888_to_rgb888 dst src (fromIntegral w) (fromIntegral h)))

