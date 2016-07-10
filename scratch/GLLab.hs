
module GLLab where

import GLTest
import Graphics.UI.WX as WX
import Graphics.UI.WXCore
import qualified Graphics.UI.WXCore as WXC
import Data.IORef

import ScriptEditView
import GLView
import Signals
import GLModule
import List
import Control.Exception

import DeepSeq

--import qualified Graphics.Rendering.OpenGL as GL


main ::
  (forall a . String -> String -> IO (a, IO ())) ->
  IO ()
main eval = start $ do
-- main = start $ do
  f <- frame [ text := "GL Lab" ]

  bus <- initBus

  reload_ref <- newIORef Nothing

  splitter <- splitterWindow f []
  code <- scriptEditView bus splitter
  set f [ layout := (grid 5 5 [ [fill $ widget splitter]]) ]

  monitor bus "reload-code" $ \(fn,cts) -> do
    try $ do
      Just (ms,unload,gl) <- readIORef reload_ref
      sequence_ ms
      print 1
--      WXC.windowRemoveChild splitter gl
      print 2
      unload
      print 3
    let _ = cts :: String
        _ = fn :: String
--    writeFile fn cts
    print 3
--    (gl_module,unload) <- eval fn "gl_module" :: IO (IO GLModule, IO ())
    let unload = return ()
    print 4
    glm <- gl_module
    print 5
    mv1 <- monitor bus "gl-create" $ \() -> gl_create glm
    mv2 <-monitor bus "gl-paint"  $ \() -> gl_display glm
    mv3 <- monitor bus "gl-resize" $ \(w,h) -> gl_resize glm w h
    print 6
  --  (glView,unload) <- eval fn "createWidget"
    gl <- glView (prefix bus "gl") splitter 200 200
    print 7
    signal bus "gl-create" ()
    print 8
    writeIORef reload_ref $ Just ([mv1,mv2,mv3],unload,gl)
    print 9
    set f [ layout := fill $ vsplit splitter 5 200 (fill $ widget gl) (fill $ widget code) ]
    print 10

  let fn = "GLTest.hs"
  cts <- readFile fn
  signal bus "edit-code" (fn,deepSeq cts cts)
  signal bus "reload-code" (fn,deepSeq cts cts)
  signal bus "reload-code" (fn,deepSeq cts cts)

--  signal bus "gl-create" ()
  return ()









