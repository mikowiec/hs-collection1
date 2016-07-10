
module GLView where

import Graphics.UI.WXCore
import Graphics.UI.WX as WX
import Graphics.Rendering.OpenGL as GL

import Signals
-- import GLTest

createWidget :: Bus -> SplitterWindow () -> Int -> Int -> IO (GLCanvas ())
createWidget bus f w h = glView bus f w h

glView :: Bus -> SplitterWindow () -> Int -> Int -> IO (GLCanvas ())
glView bus f w h = do
  canvas <- glCanvasCreateEx f 0 (Rect 0 0 w h)
              0 "GL" [GL_RGBA] nullPalette
  glCanvasSetCurrent canvas
  -- m <- myInit
  WX.set canvas
    [ on paintRaw := paintGL canvas bus,
      on resize :~ \c -> resizeGL canvas bus >> c ]
  return (canvas)

paintGL canvas bus dc rect _ = do
  print "PAINT"
  glCanvasSetCurrent canvas
  signal bus "paint" ()
  -- display m
  glCanvasSwapBuffers canvas

resizeGL canvas bus = do
  print "RESIZE"
  WX.Size w h <- WX.get canvas WX.size
  signal bus "resize" (w,h)
  -- reshape m w h
  






