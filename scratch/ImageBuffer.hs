
module ImageBuffer where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Foreign.Marshal.Alloc
import Foreign.Storable

import Foreign.Ptr
import Data.Word

data ImageBuffer = ImageBuffer Int Int (Image ()) (Ptr Word8)

imageOfBuffer (ImageBuffer _ _ img _) = img

createImageBuffer :: Size -> IO ImageBuffer
createImageBuffer size = do
  let w = sizeW size; h = sizeH size
  buf <- mallocBytes (w*h*3)
  img <- imageCreateFromDataEx size buf False
  return (ImageBuffer w h img buf)

destroyImageBuffer (ImageBuffer _ _ img p) = do
  imageDestroy img
  free p

withImageBuffer (ImageBuffer w h _ p) f = do
  mapM_ g [(x,y) | x <- [0..w-1], y <- [0..h-1] ]
 where g (x, y) = do
        let o = 3*(y*w + x)
        r <- peekElemOff p (o+0)
        g <- peekElemOff p (o+1)
        b <- peekElemOff p (o+2)
        (r,g,b) <- f x y r g b
        pokeElemOff p (o+0) r
        pokeElemOff p (o+1) g
        pokeElemOff p (o+2) b

