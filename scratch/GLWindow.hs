
module GLWindow where

import Data.List ( transpose )
import Graphics.UI.WXCore
import Graphics.UI.WX as WX
import Graphics.Rendering.OpenGL as GL

type Config = [[GL.Vertex3 GL.GLfloat]]

type State = Map

type Map = GLmap2 GL.Vertex3 GL.GLfloat

-- create :: Window a -> (IO Config) -> IO (Window (CGLCanvas ()), IO ())
create f = do
   canvas <- glCanvasCreateEx f 0 (Rect 0 0 200 200)
        0 "GLCanvas" [GL_RGBA] nullPalette
   let update = do
        glCanvasSetCurrent canvas
        m <- initCtrl
        WX.set canvas [ on paintRaw := paintGL canvas m ]
        return ()
   update
   return (canvas, update)

paintGL :: GLCanvas a -> Map -> DC () -> WX.Rect -> [WX.Rect]-> IO ()
paintGL gl1 m dc myrect _ = do

   glCanvasSetCurrent gl1
   myInit
   GL.map2 GL.$= Just (m :: GLmap2 GL.Vertex3 GL.GLfloat)
   reshape $ convWG $ rectSize myrect
   GL.clearColor GL.$= GL.Color4 1 0 0 0
   display
   glCanvasSwapBuffers gl1

   return ()

convWG (WX.Size w h) = (GL.Size (convInt32  w) (convInt32  h))
convInt32 = fromInteger . toInteger



ctrlPoints :: [[GL.Vertex3 GL.GLfloat]]
ctrlPoints = [
   [ GL.Vertex3 (-1.5) (-1.5)   4.0,  GL.Vertex3 (-0.5) (-1.5)   2.0,
     GL.Vertex3   0.5  (-1.5) (-1.0), GL.Vertex3   1.5  (-1.5)   2.0 ],
   [ GL.Vertex3 (-1.5) (-0.5)   1.0,  GL.Vertex3 (-0.5) (-0.5)   3.0,
     GL.Vertex3   0.5  (-0.5)   0.0,  GL.Vertex3   1.5  (-0.5) (-1.0) ],
   [ GL.Vertex3 (-1.5)   0.5    4.0,  GL.Vertex3 (-0.5)   0.5    0.0,
     GL.Vertex3   0.5    0.5    3.0,  GL.Vertex3   1.5    0.5    4.0 ],
   [ GL.Vertex3 (-1.5)   1.5  (-2.0), GL.Vertex3 (-0.5)   1.5  (-2.0),
     GL.Vertex3   0.5    1.5    0.0,  GL.Vertex3   1.5    1.5  (-1.0) ]]

initlights :: IO ()
initlights = do
   GL.lighting GL.$= GL.Enabled
   GL.light (GL.Light 0) GL.$= GL.Enabled

   GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.2 0.2 0.2 1.0
   GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 2 1

   GL.materialDiffuse   GL.Front GL.$= GL.Color4 0.6 0.6 0.6 1.0
   GL.materialSpecular  GL.Front GL.$= GL.Color4 1.0 1.0 1.0 1.0
   GL.materialShininess GL.Front GL.$= 50


initCtrl :: IO Map
initCtrl =  GL.newMap2 (0, 1) (0, 1) (transpose ctrlPoints)


myInit = do
--   GL.clearColor GL.$= GL.Color4 1 0 0 0
   GL.depthFunc GL.$= Just GL.Less
   GL.autoNormal GL.$= GL.Enabled
   mapGrid2 GL.$= ((20, (0, 1)), (20, (0, 1 :: GL.GLfloat)))
   initlights  -- for lighted version only

display = do
   GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
   GL.preservingMatrix $ do
     GL.rotate (85 :: GL.GLfloat) (GL.Vector3 1 1 1)
     evalMesh2 Fill (0, 20) (0, 20)
   GL.flush

reshape mysize@(GL.Size w h) = do
   GL.viewport GL.$= (GL.Position 0 0, mysize)
   GL.matrixMode GL.$= GL.Projection
   GL.loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then GL.ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else GL.ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   GL.matrixMode GL.$= GL.Modelview 0
   GL.loadIdentity

