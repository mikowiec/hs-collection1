
module GLTest where

import Data.IORef
import Data.List ( transpose )
import Graphics.Rendering.OpenGL as GL
import GLModule

type Config = [[GL.Vertex3 GL.GLfloat]]

type State = Map

type Map = GLmap2 GL.Vertex3 GL.GLfloat

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

   GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.2 0.8 0.2 1.0
   GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 2 1

   GL.materialDiffuse   GL.Front GL.$= GL.Color4 0.6 0.6 0.6 1.0
   GL.materialSpecular  GL.Front GL.$= GL.Color4 1.0 1.0 1.0 1.0
   GL.materialShininess GL.Front GL.$= 50


myInit = do
   GL.clearColor GL.$= GL.Color4 1 0 0 0
   m <- GL.newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   GL.depthFunc GL.$= Just GL.Less
   GL.autoNormal GL.$= GL.Enabled
   mapGrid2 GL.$= ((20, (0, 1)), (20, (0, 1 :: GL.GLfloat)))
   initlights  -- for lighted version only
   return m

display m = do
   GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
   GL.map2 GL.$= Just (m :: GL.GLmap2 GL.Vertex3 GL.GLfloat)
   GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
   GL.preservingMatrix $ do
     GL.rotate (85 :: GL.GLfloat) (GL.Vector3 1 1 1)
     evalMesh2 Fill (0, 20) (0, 20)
   GL.flush

reshape :: Map -> Int -> Int -> IO ()
reshape _ w h = do
   let mysize = GL.Size (fromIntegral w) (fromIntegral h)
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


gl_module :: IO GLModule
gl_module = do
  ref <- newIORef Nothing
  return $ 
   GLModule 
    (GLMODULE ref myInit display reshape)



