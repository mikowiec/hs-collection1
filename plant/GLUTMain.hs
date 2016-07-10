


module Main where

import Prelude hiding (init)

import Monad  (when, unless)
import System (getArgs, exitWith, ExitCode(..))

import GL
import GLU
import GLUT

import IOExts
import Texture
import GLView
import GeomRep
import MonadLib
import NonBasic
import Run
import MonInter(msimula)
import Misc

--type ProcGeomRep = Either GeomRep (GeomRep,GeomRep)

gl_draw :: (Tree -> (ProcGeomRep -> IO (GeomRep,GeomRep)) -> IO Tree) ->
           (ProcGeomRep -> Textures -> IO (GeomRep,GeomRep)) ->
           (IORef (Tree,Textures), IORef (Float,Float,Float,Bool)) -> IO ()
gl_draw draw gl (sim_ref,disp_ref) = do
    clearColor (Color4 0 0 0 1)
    clear [ColorBufferBit, DepthBufferBit]
    loadIdentity
    enable CullFace
    enable DepthTest
    enable Lighting
    enable (Light 0)
    blendFunc SrcAlpha OneMinusSrcAlpha

    light (Light 0) (Position (Vertex4 5.0 5.0 5.0 0.0))
    light (Light 0) (LightColor Ambient (Color4 1.0 1.0 1.0 0.0))   
    mapM_ (material Front) [ MaterialColor Specular (Color4 1 1 1 0),
   			                 MaterialColor Diffuse (Color4 0.6 0.4 0.2 0),
   			                 MaterialColor Ambient (Color4 0.2 0.2 0.2 0),
   			                 MaterialColor Emission (Color4 0 0 0 0),
                             Shininess 0.1]
    lookAt (0,5,10) (0,5,0) (0,1,0)
    (y_rot,x_rot,z_rot,l) <- readIORef disp_ref
    rotate x_rot (Vector3 1 0 0)
    rotate y_rot (Vector3 0 1 0)
    rotate z_rot (Vector3 0 0 1)
    pointSize 5
    (tree,ts) <- readIORef sim_ref
    tree' <- draw tree (`gl` ts)
    writeIORef sim_ref (tree',ts)
    swapBuffers

muu = "tjo"

gl_reshape (WindowSize width height) = do
    viewport (WindowPosition 0 0) (WindowSize width height)
    matrixMode Projection
    loadIdentity
    perspective 80.0 (fromIntegral width/fromIntegral height) 1.0 50.0
    matrixMode Modelview
    loadIdentity


simul ref = do
    (tree,ts) <- readIORef ref
    putStr ( st_err $ tree_state tree )    
    let st = tree_state tree
        nst = st {st_err = ""}
        ntree = tree {tree_state = nst}
    let (_,tree') =  runst Run.simulate ntree

    writeIORef ref (tree',ts)
    return True

gl_key (sim_ref, disp_ref) key wp = do
    case key of
     '\027' -> GLUT.exitWith ExitSuccess
     'k' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y+5,z,l))
     'i' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y-5,z,l))
     'j' -> modifyIORef disp_ref (\(x,y,z,l) -> (x+5,y,z,l))
     'l' -> modifyIORef disp_ref (\(x,y,z,l) -> (x-5,y,z,l))
     'n' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z+5,l))
     'm' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z-5,l))
     'y' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z, not l))
     _   -> simul sim_ref >> return ()
    postRedisplay


main :: IO ()
main  = do
    (progName, args) <- GLUT.init Nothing
    createWindow "plantlab" (return ())
                 [ GLUT.Double, GLUT.Rgba ]
                 Nothing (Just $ WindowSize 800 600)

    tree <- Run.init_tree
    ts <- texInit ["ost"]
    sim_ref <- newIORef (tree,ts)
    simul sim_ref
    disp_ref <- newIORef (0.0::Float,0.0::Float,0.0::Float,True::Bool)
    displayFunc (gl_draw Run.run_draw GLView.draw (sim_ref,disp_ref))
    keyboardFunc (Just (gl_key (sim_ref, disp_ref)))
    reshapeFunc (Just gl_reshape)
--    let f = (simul sim_ref >> postRedisplay >> timerFunc 1000 f)
    mainLoop


