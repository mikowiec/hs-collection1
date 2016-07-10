
module GLMisc where

import qualified GL
import qualified GLU
import GL (($=))
import Graphics.Rendering.OpenGL.GL.Capability as Cap

import LinAlg
import Monad
import GLHelper

blending = Cap.makeCapability Cap.CapBlend
depthTest = Cap.makeCapability Cap.CapDepthTest

clear_view = do
    GL.clearColor $= (GL.Color4 0.0 0.0 0.2 1)
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

rect v0 v1 v2 = do
    mapM_ vertex3 (map (add v0) [(0,0,0), v1, v2, v1, v1 `add` v2, v2])

plane b ori norm s = do
    let (a1,a2) = mk_axises norm
    let (v1,v2) = (s `mulk` a1, s `mulk` a2)
    let c = 0.5 `mulk` (v1 `add` v2)
    let ll = ori `sub` c
    rect ll v1 v2
    when b $ do
        color (1,1,1,1)
        let ll' = ll `add` (0.003 `mulk` norm)
        let bands v1 v2 cv = do
            let d = 0.1 `mulk` v1
            let w = 0.004 `mulk` v1
            mapM_ (\i -> cv (rect (ll' `add` (i `mulk` d))) w v2) [0..10]
        bands v1 v2 id
        bands v2 v1 flip


draw_ground = do
    GL.texture GL.Texture2D $= GL.Disabled
    GL.cullFace $= Nothing
    blending $= GL.Disabled
    GL.lighting $= GL.Disabled
    GL.renderPrimitive GL.Triangles $ do
        color (0.3,0.3,0.1,0.7)
        plane False (0,-0.01,0) (0,1,0) 1000


draw_grid side = do
    GL.texture GL.Texture2D $= GL.Disabled
    GL.cullFace $= Just GL.Back
    blending $= GL.Disabled
    GL.lighting $= GL.Disabled
    GL.translate (GL.Vector3 0 (side/2) 0)
    GL.renderPrimitive GL.Triangles $ do
        color (0.3,0.3,0.1,1.0)
        plane True (0,-side/2,0) (0,1,0) side
        color (0.3,0.3,0.5,1.0)
        plane True (0,side/2,0) (0,-1,0) side
        color (0.3,0.4,0.2,1.0)
        plane True (side/2,0,0) (-1,0,0) side
        color (0.3,0.4,0.2,1.0)
        plane True (-side/2,0,0) (1,0,0) side
        color (0.3,0.4,0.2,1.0)
        plane True (0,0,side/2) (0,0,-1) side
        color (0.3,0.4,0.2,1.0)
        plane True (0,0,-side/2) (0,0,1) side
    GL.translate (GL.Vector3 0 (-side/2) 0)


simple_cyl p1@(px,py,pz) p2 = do
    let lv = p2 `sub` p1
        z_axis = (0, 0, 1)
        (angle,axis) = mk_rot_axis z_axis lv
        rot_axis = vec3 (norm (cross z_axis lv))
        rot_angle = r2d $ ang z_axis lv
    GL.translate (GL.Vector3 (px) (py) (pz))
    GL.rotate (r2d angle) (vec3 axis)
    color (let (r,g,b) = norm p2 in (r,g,b,0.7))
    cylinder 0.01 0.01 (realToFrac (absval lv)) 5 5

draw_target pos = do
    draw_cross pos 0.5
    draw_sphere pos 0.2
    return ()


draw_sphere pos rad = do
    blending $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Disabled
    GL.lighting $= GL.Disabled
    depthTest $= GL.Enabled
    GL.cullFace $= Just GL.Back
    with_push_mtx $ do
        GL.translate (vec3 pos)
        color (0.5,0.5,0.5,0.7)
        sphere rad 10 10
    return ()

draw_cross pos l = do
    blending $= GL.Disabled
    GL.texture GL.Texture2D $= GL.Disabled
    GL.lighting $= GL.Disabled
    GL.cullFace $= Just GL.Back
    with_push_mtx $ do
        GL.translate (vec3 pos)
        color (1,1,1,1)
        GL.renderPrimitive GL.Lines $ do
            vertex3 (-l,0,0)
            vertex3 ( l,0,0)
            vertex3 (0,-l,0)
            vertex3 (0, l,0)
            vertex3 (0,0,-l)
            vertex3 (0,0, l)
    return ()

draw_coords pos = do
    blending $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Disabled
    GL.lighting $= GL.Disabled
    GL.cullFace $= Just GL.Back
    with_push_mtx $ do
        GL.translate (vec3 pos)
        mapM_ (with_push_mtx . simple_cyl (0,0,0)) [(1,0,0), (0,1,0), (0,0,1)]
    return ()

get_pick_line x y = do
    (wp@(wpx,wpy),ws@(wsw,wsh)) <- get_viewport
    let (ww,wh) = get_vp_size ws
    pm <- GL.get $ GL.matrix (Just GL.Projection) :: IO (GL.GLmatrix GL.GLdouble)
    vm <- GL.get $ GL.matrix (Just (GL.Modelview 0))
    let ry = (fromIntegral wh) - y - 1
    GL.Vertex3 x1 y1 z1 <- GLU.unProject (GL.Vertex3 (realToFrac x) (realToFrac ry) 0) vm pm (GL.Position wpx wpy, GL.Size wsw wsh)
    GL.Vertex3 x2 y2 z2 <- GLU.unProject (GL.Vertex3 (realToFrac x) (realToFrac ry) 0.1) vm pm (GL.Position wpx wpy, GL.Size wsw wsh)
    let f = realToFrac
    return ((f x1,f y1,f z1), (f x2,f y2,f z2))


draw_tex_rect_ori pt0 v sz = do
    let b = (0.5 * sz) `mulk` (norm ( cross v dirup ) )
        pt1 = ( pt0 `add` (sz `mulk` v) )
    draw_tex_rect pt0 pt1 b

draw_multi_tex_rect_ori pt0 v sz = do
    let b = (0.5 * sz) `mulk` (norm ( cross v dirup ) )
        pt1 = ( pt0 `add` (sz `mulk` v) )
    draw_multi_tex_rect pt0 pt1 b

draw_tex_rect pt0 pt1 b = do
        let p0 = pt0 `sub` b
            p1 = pt1 `sub` b
            p2 = pt1 `add` b
            p3 = pt0 `add` b
            
        tex2 (0.0, 0.0); vertex3 p0
        tex2 (0.0, 1.0); vertex3 p1
        tex2 (1.0, 1.0); vertex3 p2 
        
        tex2 (1.0, 1.0); vertex3 p2
        tex2 (1.0, 0.0); vertex3 p3
        tex2 (0.0, 0.0); vertex3 p0 

draw_multi_tex_rect pt0 pt1 b = do
        let p0 = pt0 `sub` b
            p1 = pt1 `sub` b
            p2 = pt1 `add` b
            p3 = pt0 `add` b

        m2tex2 (0.0, 0.0); vertex3 p0
        m2tex2 (0.0, 1.0); vertex3 p1
        m2tex2 (1.0, 1.0); vertex3 p2 

        m2tex2 (1.0, 1.0); vertex3 p2
        m2tex2 (1.0, 0.0); vertex3 p3
        m2tex2 (0.0, 0.0); vertex3 p0 




init_view width height = do
    GL.viewport $= (GL.Position 0 0, GL.Size width height)
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GLU.perspective 80.0 (fromIntegral width/fromIntegral height) 0.1 500.0
    GL.matrixMode $= GL.Modelview 0
    GL.loadIdentity
    return ()


init_gl = do
    GL.cullFace $= Just GL.Back
    depthTest $= GL.Enabled
    GL.lighting $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Enabled
    GL.light (GL.Light 0) $= GL.Enabled
    blending $= GL.Enabled
    GL.autoNormal $= GL.Enabled
    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.ambient (GL.Light 0) $= GL.Color4 0.2 0.2 0.2 1.0
    GL.diffuse (GL.Light 0) $= GL.Color4 1.0 1.0 1.0 1.0
    GL.position (GL.Light 0) $= GL.Vertex4 5.0 5.0 5.0 0.0
    GL.materialAmbient GL.FrontAndBack $= GL.Color4 0.2 0.2 0.2 1
    GL.materialSpecular GL.FrontAndBack $= GL.Color4 0.4 0.4 0.4 1
    GL.materialDiffuse GL.FrontAndBack $= GL.Color4 0.3 0.3 0.3 1
    GL.materialEmission GL.FrontAndBack $= GL.Color4 0.0 0.0 0.0 0
    GL.materialShininess GL.FrontAndBack $= 0.1


setup_view pos ori tgt = do
    GL.loadIdentity
    translate ((-1) `mulk` pos)
    let (v,axis) = q2axis ori
    GL.rotate (57*v) (vec3 axis)
    translate ((-1) `mulk` tgt)
    return ()

