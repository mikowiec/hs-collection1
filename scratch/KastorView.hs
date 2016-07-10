
module KastorView where

import Monad
import Graphics.UI.WX as WX hiding (when)

import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import Data.IORef
import Data.Word
import Data.Bits
import Text.Printf

import KTypes
import KInterface

import KastorInstance

import Profile
import ImageBuffer
import IO
import Utils
import Blit

data KastorView = KastorView Kastor (Window ()) ImageBuffer

{-
foreign import ccall "blit_argb8888_to_rgb888" c_blit_argb8888_to_rgb888
  :: Ptr Word8 -> Ptr Word32 -> Word16 -> Word16 -> IO ()
-}

createKastorView :: Kastor -> Window a -> Int -> Int -> [Prop (Window ())] -> IO KastorView
createKastorView k f w h opts = ref_epoch `seq` do
  ref <- newIORef []
  (wnd,buf) <- createImageBufferWindow f w h opts $
   \wnd (ImageBuffer _ _ _ bufPtr) -> do
    now <- getTime
    kastorRun k (now - ref_epoch) $ \fb dr next -> do
     when (next >= 0) $ modifyIORef ref (repaint wnd:)
     blit_rgb565_to_rgb888 bufPtr fb (fromIntegral w) (fromIntegral h)
  set wnd [ on closing :~ \c -> destroyImageBuffer buf >> c ]
  t <- timer wnd [ interval := 10 ]
  set t [ on command := do cs <- readIORef ref
                           writeIORef ref []
                           sequence_ cs ]
  return (KastorView k wnd buf)

createImageBufferWindow parent w h opts f = do
  wnd <- window parent ([ size := sz w h ] ++ opts)
  buf <- createImageBuffer (sz w h)
  withImageBuffer buf (\_ _ _ _ _ -> return (0,0,0))
  let paint dc _ _ = do
       f wnd buf
       drawImage dc (imageOfBuffer buf) (Point 0 0) []
  set wnd [ on paintRaw := paint ]
  return (wnd,buf)


kv_main = start $ do
  let w = 240; h = 320
  f <- frame [ text := "KastorView", size := sz w h ]

  k <- createKastor w h
  
  kv@(KastorView _ wnd buf) <- createKastorView k f w h []
  withKastor k setupSimpleKastorTest
  t <- timer wnd [ interval := 1000 ]
  set t [ on command := repaint wnd ]

  let shutdown = destroyKastor k >> return ()
  set f [ on closing :~ \c -> shutdown >> c ]
  
  set f [ layout := fill (widget wnd) ]

  return ()


setupSimpleKastorTest core = do
  root <- kcore_createNode core "svg"
  rect <- kcore_createNode core "rect"
  knode_setAttrs root [(KID_width, AttrX 240), (KID_height, AttrX 320)]
  knode_setAttrs rect
    [(KID_x, AttrX 120),
     (KID_y, AttrX 100),
     (KID_width, AttrX 50),
     (KID_height, AttrX 30),
     (KID_fill, AttrColor 0xffff0000)]

  knode_appendChild root rect
  knode_release rect
  kcore_setRoot core root
  knode_release root


