


module Main where

import Prelude hiding (init)
import Char

import Monad  (when, unless)
import System (getArgs, exitWith, ExitCode(..))

import GL
import GLU

import Povray2 (geomrep2povray2)

import LinAlg
import IOExts
import Texture
import GLView
import GeomRep
import MonadLib
import NonBasic
import Run
import MonInter(msimula)
import Misc
import PlantTypes
import NumExts
import STArrLib (lift)

import Macro

import qualified Gdk
import qualified Gtk
import qualified GdkGL
import qualified GtkGL


-- ta bort denna sen
stp::String -> IO ()
stp txt = do putStr txt
             char <- getChar
             print [char]
             return ()

--type ProcGeomRep = Either GeomRep (GeomRep,GeomRep)

gl_init ref wd = do
    ts <- texInit ["leaf1","tuja","leaf1faded"]
    modifyIORef ref (\(a,b,_) -> (a,b,ts))
    gl_reshape wd
    return ()

    
clone sim_ref = do        
        (tree, rep, ts) <- readIORef sim_ref
        print "Cloning..."
        ntree <- ioclone tree
        print "Done cloning."
        writeIORef sim_ref (ntree, rep, ts)
       



dry_draw :: (IORef (Tree ,Maybe ExecRep,Textures), IORef (Float,Float,Float,Bool)) -> IO ()
dry_draw (sim_ref,disp_ref) = do
    print "dry draw..."
    (tree,rep,ts) <- readIORef sim_ref
    rendat <- iodraw tree
    let ntree = tree {tree_rep = Just rendat} 
    writeIORef sim_ref (ntree, rep, ts)
    print "dry draw done"
    

gl_draw :: (IORef (Tree ,Maybe ExecRep,Textures), IORef (Float,Float,Float,Bool)) -> IO ()
gl_draw (sim_ref,disp_ref) = do
    print "render..."
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
    (tree,rep,ts) <- readIORef sim_ref
    rendat <- case (tree_rep tree) of
                 Nothing -> iodraw tree
                 Just rep -> return rep

    let ntree = tree {tree_rep = Just rendat} 
    writeIORef sim_ref (ntree, rep, ts)
    
    -- let rendat = head $ greps
    let grep = rd_geomrep rendat
   -- let psize = 10.0 -- bs_dev $ head $ st_bs $ tree_state tree
   --     zback = (floatToDouble psize) + 0.0
   --     yup = (floatToDouble $ psize / 2.0)
    lookAt (rd_campos rendat) (rd_camlookat rendat) (0,1,0)
    (y_rot,x_rot,z_rot,l) <- readIORef disp_ref
    rotate x_rot (Vector3 1 0 0)
    rotate y_rot (Vector3 0 1 0)
    rotate z_rot (Vector3 0 0 1)
    pointSize 5
--    rep' <- Run.run_draw tree rep (`GLView.draw` )
--    let tex = head $ co_tex $ se_comin $ ist_seed $ tree_state tree
--    bind_texture tex ts 
    rep' <- GLView.simple_draw grep ts
    return ()
--    writeIORef sim_ref (greps,(Just rep'),ts)
    print "render complete."

gl_reshape wd = do
    (_,_,width,height) <- Gtk.widgetGetAllocation wd
    viewport (WindowPosition 0 0) (WindowSize width height)
    matrixMode Projection
    loadIdentity
    perspective 80.0 (fromIntegral width/fromIntegral height) 0.1 50.0
    matrixMode Modelview
    loadIdentity


simul ref = do
    error "do not use simul anymore!"
    (tree,rep,ts) <- readIORef ref
    let st = tree_state tree
        nst = st 
        ntree = tree {tree_state = nst}
    -- tree' <- lift $ (do (_,t) <- runstp Run.simulate ntree
    --                   return t)

    -- writeIORef ref (tree',Nothing,ts)
    return True

simul2 :: IORef (Tree, Maybe ExecRep, Textures) -> IO ()
simul2 ref = do        
        (tree, rep, ts) <- readIORef ref
        ntree <- iosim tree
        writeIORef ref (ntree, rep, ts)

simul3 :: Int -> IORef (Tree, Maybe ExecRep, Textures) -> IO ()
simul3 0 _ = return ()
simul3 n ref = do
        (tree, rep, ts) <- readIORef ref
        ntree <- iosim tree
        writeIORef ref (ntree, rep, ts)
        simul3 (n-1) ref

process_macro refs [] = return ()
process_macro refs ((Evolve x):xs) = do simul3 x (fst refs)
                                        process_macro refs xs
process_macro refs ((PovEx x):xs) = do writePovFile refs x
                                       process_macro refs xs
process_macro refs ((DryRen):xs) = do dry_draw refs
                                      process_macro refs xs
process_macro refs ((SetRes x):xs) = do setRes refs x                                     
                                        process_macro refs xs
process_macro refs (Clone:xs) = do clone (fst refs)                                       
                                   process_macro refs xs

process_key refs@(sim_ref, disp_ref) k = do
 case chr k of
           ' ' -> simul2 sim_ref >> return ()
           'k' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y+5,z,l))
           'i' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y-5,z,l))
           'j' -> modifyIORef disp_ref (\(x,y,z,l) -> (x+5,y,z,l))
           'l' -> modifyIORef disp_ref (\(x,y,z,l) -> (x-5,y,z,l))
           'n' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z+5,l))
           'm' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z-5,l))
           'y' -> modifyIORef disp_ref (\(x,y,z,l) -> (x,y,z, not l))
           'd' -> dry_draw (sim_ref,disp_ref)
           'g' -> do print "Enter new precision: "
                     iv <- getLine
                     setRes refs (read iv)
                    
           'p' -> do writePovFile refs "povray.3d"

           'e' -> do print "Enter nr of steps:"
                     iv <- getLine
                     simul3 (read iv) sim_ref

           'r' -> do gl_draw (sim_ref,disp_ref)
                     
           'o' -> do process_macro refs macra

           'c' -> do clone sim_ref
 
           _ -> return ()       


setRes refs@(sim_ref, disp_ref) res = do 
        (tree, a, b) <- readIORef sim_ref
        let st = tree_state tree
            newst = st {ist_grpr = res}
            ntree = tree {tree_state = newst}
        writeIORef sim_ref (ntree, a, b)


writePovFile (sim_ref, disp_ref) fn = do 
     print $ "writing povray... " ++ fn
     (tree, _, _) <- readIORef sim_ref
     case (tree_rep tree) of
        Nothing -> print "Nothing To Export"
        Just rendat -> do let campos = (\(x,y,z) -> (doubleToFloat x, (doubleToFloat y), (1.7) * ( doubleToFloat z))) (rd_campos rendat)
                              camlookat = (\(x,y,z) -> (doubleToFloat x, (doubleToFloat y), (1.7) * ( doubleToFloat z))) (rd_camlookat $ rendat)
                              povrep = geomrep2povray2 campos camlookat id_quat (rd_geomrep rendat)
                          writeFile fn povrep
                          print ("povray file " ++ fn ++ " written")
                          print $ show campos
     return ()
 
gl_key refs state key = do
    k <- Gdk.eventKeyGetKey key
    if k > 255 then case k of
                     65307 -> Gtk.mainQuit
                     _ -> return ()
               else when (state == True) (process_key refs k)
      

gl_ok ret gl wd _ = do
    allowed <- GtkGL.areaMakeCurrent wd
    when allowed gl
    GtkGL.areaSwapbuffers wd
    return ret

main :: IO ()
main  = do
    Gtk.init Nothing

    supported <- GtkGL.query
    unless supported $ do
      putStr "OpenGL not supported.\n"
      Gtk.exit (ExitFailure 1)

    window <- Gtk.windowNew Gtk.WindowToplevel
    Gtk.windowSetDefaultSize window 1200 1000
    Gtk.windowSetTitle window "gtkgl"
    Gtk.signalConnect window $ Gtk.WidgetDeleteEventHandler $ \_ _ -> Gtk.mainQuit >> return False
    Gtk.quitAddDestroy 1 window

    glarea <- GtkGL.areaNew [GdkGL.Rgba, GdkGL.Doublebuffer]

    Gtk.widgetSetEvents window [Gdk.ButtonPressMask,
                                Gdk.ButtonReleaseMask, Gdk.PointerMotionMask,
                                Gdk.KeyPressMask, Gdk.KeyReleaseMask]

    Gtk.widgetSetEvents glarea [Gdk.ExposureMask, Gdk.ButtonPressMask,
                                Gdk.ButtonReleaseMask, Gdk.PointerMotionMask,
                                Gdk.KeyPressMask, Gdk.KeyReleaseMask]

    init_tree <- Run.gen_tree
    sim_ref <- newIORef (init_tree,Nothing,[])
    -- simul sim_ref
    disp_ref <- newIORef (0.0::Float,0.0::Float,0.0::Float,True::Bool)

    Gtk.signalConnect glarea $ Gtk.WidgetExposeEventHandler $
            gl_ok True (gl_draw (sim_ref,disp_ref))
    Gtk.signalConnect glarea $ Gtk.WidgetConfigureEventHandler $
            (\wd b -> do (_,_,w,h) <- Gtk.widgetGetAllocation wd
                         gl_ok True (gl_reshape wd) wd b)
    Gtk.signalConnect glarea $ Gtk.WidgetRealizeHandler $
        \wd -> gl_ok () (gl_init sim_ref wd) wd ()
    Gtk.signalConnect window $ Gtk.WidgetKeyPressEventHandler $
        \_ k -> gl_ok True (gl_key (sim_ref,disp_ref) True k) glarea k
    Gtk.signalConnect window $ Gtk.WidgetKeyReleaseEventHandler $
        \_ k -> gl_ok True (gl_key (sim_ref,disp_ref) False k) glarea k

    Gtk.containerAdd window glarea
    Gtk.widgetShowAll window
    Gtk.main
    exitWith ExitSuccess


