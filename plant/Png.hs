
module Png(Png(..), decode, withPng, load, free, CPng) where

import CForeign
import CString
import Ptr
import Word



data Png = Png Int Int (Ptr Word8)

decode :: Ptr CPng -> Png
decode p = Png w h (hs_png_get_data p)
 where w = fromIntegral (hs_png_get_width p)
       h = fromIntegral (hs_png_get_height p)

withPng :: String -> (Png -> IO a) -> IO a
withPng fn m = do
    p <- load fn
    let png = decode p
    a <- m png
    free p
    return a


data CPng = CPng

load :: String -> IO (Ptr CPng)
load fn = withCString fn hs_png_load

free :: (Ptr CPng) -> IO ()
free p = hs_png_free p


foreign import ccall "png/hs_png.h hs_png_load" hs_png_load :: CString -> IO (Ptr CPng)
foreign import ccall "png/hs_png.h hs_png_free" hs_png_free :: Ptr CPng -> IO ()
foreign import ccall "png/hs_png.h hs_png_get_width" hs_png_get_width :: Ptr CPng -> CInt
foreign import ccall "png/hs_png.h hs_png_get_height" hs_png_get_height :: Ptr CPng -> CInt
foreign import ccall "png/hs_png.h hs_png_get_data" hs_png_get_data :: Ptr CPng -> Ptr Word8



