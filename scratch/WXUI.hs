
module WXUI where

import Array
import Monad
import Data.IORef

import Graphics.UI.WX as WX
import Graphics.UI.WXCore

import PaintWindow
import GLWindow as GLW

import HighPrecisionTime
import Printf

import Foreign.Marshal.Alloc
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word

import ImageBuffer


main :: IO ()
main = start gui

pixels = concat
 [ [ rgb i j i | i <- [0..199] ] | j <- [0..199] ]



gui = do
  f <- frame [ text := "Simple OpenGL" ]

  img_buf <- createImageBuffer (sz 200 200)
  withImageBuffer img_buf (\_ _ _ _ _ -> return (0,0,0))
  img_wnd <- window f [ size := sz 200 200 ]
  now <- getTime
  frame_time <- newIORef now
  clr_off <- newIORef 0
  let draw_img dc _ _ = do
       now <- getTime
       last <- readIORef frame_time
       drawImage dc (imageOfBuffer img_buf) (Point 0 0) [ ]
       printf "diff %5.1f fps %5.1f\n" ((now-last)*1000) (1/(now-last))
       writeIORef frame_time now
  t <- timer img_wnd [ interval := 100 ]
  set t [ on command := do
            clr <- atomicModifyIORef clr_off (\clr -> (clr+10,clr))
            let clr_ofs = round $ 128 + 100 * sin (pi*fromIntegral clr/180)
            let f x y r g b = do
                  return (fromIntegral x,fromIntegral y, clr_ofs)
            withImageBuffer img_buf f
            repaint img_wnd ]
  set img_wnd [ on paintRaw := draw_img ]

  {-
  ctrl_points <- load "GLData"
  gl_module <- load "GLWindow"

  gl_module ctrl_points
-}

  (glCanvas,gl_update) <- GLW.create f
  pw <- paint_window f
  WX.set pw [ size := sz 200 200 ]

  btn <- button f [ text := "Run" ]

  let row_layout = fill $
        column 2 [
         row 5 [ widget btn ],
         row 5 [ widget pw, widget glCanvas, widget img_wnd ] ]

-- Hint: You have to use the paintRaw event. For switching between the two
--   glwindows you can give both of them as parameter
  WX.set f [ layout := row_layout
  --          , on paintRaw := paintGL glCanvas
            ]




