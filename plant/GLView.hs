
module GLView where

import qualified GL
import qualified GLU
import GL (($=))
import MultiTexturing

import Array
import Pf
import Misc

import Foreign

import Monad
import Maybe
import LinAlg
import GeomRep
import List

import Types

import Random

import Texture
import MonadLib

import IOExts

import GLMisc
import GLHelper

import FiniteMap

import Foreign.Marshal.Array


type ExecRep = (IO (), IO (), Int, Int)
type ProcGeomRep = Either GNode ExecRep

bind_texture t ts = do
    Just texid <- return $ lookup t ts
    GL.textureBinding GL.Texture2D $= Just texid
 `catch` (\_-> fail ("cannot find texture: " ++ t))


mk_list_exec m = mk_list_aux GL.CompileAndExecute m
mk_list_exec_ m = fst $^ mk_list_exec m
mk_list m = mk_list_aux GL.Compile m
mk_list_ m = fst $^ mk_list m

mk_list_aux mode m = do
    [i] <- GL.genLists 1
    r <- GL.defineList i GL.Compile m
    modifyIORef ?l_all (i:)
    return (i,r)

geom_eq g g' = case (g,g') of
 (Li _ _ _ _       , Li _ _ _ _       ) -> True
 (LiA _ _ _ _ _ _  , LiA _ _ _ _ _ _  ) -> True
 (Pt _ _           , Pt _ _           ) -> True
 (PtA _ _ _ s      , PtA _ _ _ s'     ) -> s == s'
 (Branch s _ _ _ _ , Branch s' _ _ _ _) -> s == s'
 (FnBranch s _ _   , FnBranch s' _ _  ) -> s == s'
 (Square s _       , Square s' _      ) -> s == s'
 (Surface s _ _ _ _, Surface s' _ _ _ _) -> s == s'
 (_                , _                ) -> False

gp2ix gp = case gp of
    Pt _ _           -> 0
    Li _ _ _ _       -> 1
    LiA _ _ _ _ _ _  -> 2
    PtA _ _ _ _      -> 3
    Branch _ _ _ _ _ -> 4
    Square _ _       -> 5
    FnBranch _ _ _   -> 6
    Surface _ _ _ _ _ -> 7

deep_cmp (PtA _ _ _ s     ) (PtA _ _ _ s'     ) = s `compare` s'
deep_cmp (Branch s _ _ _ _) (Branch s' _ _ _ _) = s `compare` s'
deep_cmp (FnBranch s _ _  ) (FnBranch s' _ _  ) = s `compare` s'
deep_cmp (Square s _      ) (Square s' _      ) = s `compare` s'
deep_cmp (Surface s _ _ _ _) (Surface s' _ _ _ _) = s `compare` s'
deep_cmp _                  _                   = EQ

geom_cmp g g'
 | g `geom_eq` g' = deep_cmp g g'
 | otherwise      = gp2ix g `compare` gp2ix g'


sort_geom gs = (groupBy geom_eq . sortBy geom_cmp) gs

draw_list [] = return ()
draw_list xs = do
    with_push_mtx $ draw_prims (head xs) xs


simple_draw :: GeomRep -> Textures -> IO ExecRep
simple_draw rep ts = draw (0,(0,-1,0)) (Left (Geometry rep)) ts


draw :: Param -> ProcGeomRep -> Textures -> IO ExecRep
draw pm rep ts = do
    case rep of
     Right (m,f,s1,s2) -> {-# SCC "draw_prep_reuse" #-} m >> return (m,f,s1,s2)
     Left gn -> {-# SCC "draw_prep_regen" #-} do
      GL.cullFace $= Just GL.Back
      GL.cullFace $= Nothing

      l_all <- newIORef []
      let ?l_all = l_all in do
      l_cyl <- mk_list_ $ cylinder 1 1 1 5 5
      l_sph <- mk_list_ $ sphere 1 5 5
      let ?ts = ts; ?l_cyl = l_cyl; ?l_sph = l_sph in do
      let gv = snd pm
      dt <- gather_draw (fst pm) gv emptyFM (Tag (-1) gn)
      let m = case lookupFM dt (-1,gv) of
               Just l -> GL.callList l
               Nothing -> return ()
      return (m, readIORef ?l_all >>= GL.deleteLists, 0, 0)

gather_draw p g dt (Param (Fn f)) = do
    gather_draw p g dt (f (p, g))
gather_draw _ _ dt (Geometry gr) = do
    mapM_ draw_list (sort_geom gr)
    return dt

gather_draw p g dt (Tag ix gn) = do
    case lookupFM dt (ix,g) of
     Nothing -> do dt' <- with_push_mtx $ gather_draw p g dt gn
                   (dl,dt'') <- with_push_mtx $ mk_list (gather_draw p g dt' gn)
                   return (addToFM dt'' (ix,g) dl)
     Just l  -> do GL.callList l
                   return dt
gather_draw p g dt (Group gs) = do
    foldM (\a b -> with_push_mtx (gather_draw p g a b)) dt gs
gather_draw p g dt gn = do
    let k g' = gather_draw p g dt g'
    case gn of
     Offset v gn -> GL.translate (vec3 v) >> k gn
     Scale (x,y,z) gn -> GL.scale x y z >> k gn
     Ori q gn -> let (v,a) = q2axis q in
                 GL.rotate (r2d v) (vec3 a) >> gather_draw p (g `rot_vec_q` (inv q)) dt gn
 


draw_prims ::
    (?l_cyl :: GL.DisplayList,
     ?l_sph :: GL.DisplayList,
     ?ts :: Textures) =>
    GeomPrim -> -- Representative element for this chunk
    GeomRep ->  -- All elements
    IO ()

draw_prims (FnBranch (Flat s@(r,g,b,a)) _ _) gs = do
    GL.texture GL.Texture2D $= GL.Enabled
    blending $= GL.Disabled
    GL.lighting $= GL.Enabled
    GL.materialSpecular GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialDiffuse GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialAmbient GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialEmission GL.FrontAndBack $= GL.Color4 0 0 0 0
    GL.materialShininess GL.FrontAndBack $= 0.1
    color s

    let draw (FnBranch _ cps (Fn sh)) = do
        let xs = [ (sh d) | d <- fill_ival cps 0.1 ]
        let ys = zip xs (tail xs)
        mapM_ (with_push_mtx . draw_branch'') (map (\((w1,p1), (w2,p2)) -> (w1,w2,p1,p2)) ys)
    mapM_ draw gs

 where draw_branch'' (w1, w2, p1@(px,py,pz), p2) = {-# SCC "draw_branch''" #-} do
        let lv = p2 `sub` p1
            z_axis = (0, 0, 1)
            (angle,axis) = mk_rot_axis z_axis lv
        GL.translate (GL.Vector3 (px) (py) (pz))
        GL.rotate (r2d angle) (vec3 axis)
        let sxy = realToFrac w1
        let len = absval lv
        GL.scale sxy sxy len
        GL.callList ?l_cyl
        GL.translate (GL.Vector3 0 0 1 :: GL.Vector3 Float)
        GL.scale 1 1 (sxy/len)
        GL.callList ?l_sph

draw_prims (Surface sur l r m t) gs = do
    let m' = t `sub` m
    let ver4 (x,y,z) = GL.Vertex4 x y z 1
    let ctrlpoints :: [[GL.Vertex4 GL.GLfloat]]
        ctrlpoints = map (map ver4) [
            [l `sub` m, (0,0,0), r `sub` m],
            [l, m, r],
            [l `add` m', t, r `add` m'] ]
    {-
        ctrlpoints = map (map ver4) [
            [(0,0,0), (1,0,0), (2,0,0), (3,0,0)],
            [(0,1,0), (1,1,0), (2,1,0), (3,1,0)],
            [(0,2,2), (1,2,0), (2,2,0), (3,2,0)],
            [(0,3,1), (1,3,0), (2,3,0), (3,3,0)],
            [(1,1,1), (2,2,2), (3,3,3), (4,4,4)]]
    -}
    let texpoints :: [[GL.TexCoord2 GL.GLfloat]]
        texpoints =
            [[GL.TexCoord2 0 0, GL.TexCoord2 1 0],
             [GL.TexCoord2 0 1, GL.TexCoord2 1 1]]

    GL.cullFace $= Nothing
    GL.lighting $= GL.Disabled
    blending $= GL.Disabled
    GL.alphaFunc $= Just (GL.Greater, 0.1)

--    let sur = Flat (1,1,1,1)
    case sur of
     Flat c -> do GL.texture GL.Texture2D $= GL.Disabled
                  color c
     Tex t -> do GL.texture GL.Texture2D $= GL.Enabled
                 bind_texture t ?ts
                 GL.textureFunction $= GL.Replace
                 -- GL.texEnv (GL.TextureEnvMode GL.Replace)
                 
     TexBlend f t1 t2 -> do
        color (f,f,f,1)

        glActiveTextureARB gl_texture1_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t1 ?ts

        glTexEnvi gl_texture_env gl_texture_env_mode gl_combine_arb
        glTexEnvi gl_texture_env gl_combine_rgb_arb gl_interpolate_arb
        glTexEnvi gl_texture_env gl_source0_rgb_arb gl_previous_arb
        glTexEnvi gl_texture_env gl_operand0_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source1_rgb_arb gl_texture
        glTexEnvi gl_texture_env gl_operand1_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source2_rgb_arb gl_primary_color_arb
        glTexEnvi gl_texture_env gl_operand2_rgb_arb gl_src_color

     
        glActiveTextureARB gl_texture0_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t2 ?ts
        GL.textureFunction $= GL.Replace

    let wrap m = do
        GL.renderPrimitive GL.Triangles m
        GL.alphaFunc $= Nothing
        glActiveTextureARB gl_texture1_arb
        GL.texture GL.Texture2D $= GL.Disabled
        glActiveTextureARB gl_texture0_arb
        GL.texture GL.Texture2D $= GL.Disabled
    let draw (Surface _ _ _ m _) = do
        let s = absval m
        let v = (0,s,0)
            b = (0,0,s*0.5)
            pt0 = (0,0,0)
            pt1 = pt0 `add` v
{-
        GL.withArray (concat ctrlpoints) $
            glMapControlPointsNV gl_eval_2d_nv 0 gl_float (4*4) (3*4*4) 3 3 0 . GL.castPtr
        GL.withArray texpoints $
            glMapControlPointsNV gl_eval_2d_nv 8 gl_float (4*4) (2*4*4) 2 2 0 . GL.castPtr
        GL.withArray texpoints $
            glMapControlPointsNV gl_eval_2d_nv 9 gl_float (4*4) (2*4*4) 2 2 0 . GL.castPtr
        e <- GL.get GL.VarError :: IO GL.Error
        glEnable gl_eval_fractional_tessellation_nv
        glEnable gl_eval_vertex_attrib0_nv
        glEnable gl_eval_vertex_attrib8_nv
        glEnable gl_eval_vertex_attrib9_nv
        glEvalMapsNV gl_eval_2d_nv gl_fill
        e <- GL.get GL.VarError :: IO GL.Error
        (GLU.errorString e) >>= print
-}
        mc <- GL.newMap2 (0,1) (0,1) ctrlpoints :: IO (GL.GLmap2 GL.Vertex4 GL.GLfloat)
        GL.map2 $= Just mc
        tc <- GL.newMap2 (0,1) (0,1) texpoints :: IO (GL.GLmap2 GL.TexCoord2 GL.GLfloat)
        GL.map2 $= Just tc
        {-
        withArray (concat ctrlpoints) $
            GL.map2 GL.Map2Vertex4 0 (1::GL.GLfloat) 4 3 0 1 12 3 . GL.castPtr
        GL.withArray texpoints $
            GL.map2 GL.Map2TextureCoord2 0 (1::GL.GLfloat) 2 2 0 1 4 2 . GL.castPtr
        GL.enable GL.Map2Vertex4
        GL.enable GL.Map2TextureCoord2
        -}
        GL.mapGrid2 $= ((20, (0::GL.GLfloat, 1)), (20, (0, 1)))
        GL.evalMesh2 GL.Fill (0, 20) (0, 20)

    wrap (mapM_ draw gs)

draw_prims (Square sur _) gs = do

    GL.cullFace $= Nothing
    GL.lighting $= GL.Disabled
    blending $= GL.Disabled
    GL.alphaFunc $= Just (GL.Greater, 0.1)
    case sur of
     Flat c -> do GL.texture GL.Texture2D $= GL.Disabled
                  color c
     Tex t -> do GL.texture GL.Texture2D $= GL.Enabled
                 bind_texture t ?ts
                 GL.textureFunction $= GL.Replace
                 
     TexBlend f t1 t2 -> do
        color (f,f,f,1)

        glActiveTextureARB gl_texture1_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t1 ?ts

        glTexEnvi gl_texture_env gl_texture_env_mode gl_combine_arb
        glTexEnvi gl_texture_env gl_combine_rgb_arb gl_interpolate_arb
        glTexEnvi gl_texture_env gl_source0_rgb_arb gl_previous_arb
        glTexEnvi gl_texture_env gl_operand0_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source1_rgb_arb gl_texture
        glTexEnvi gl_texture_env gl_operand1_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source2_rgb_arb gl_primary_color_arb
        glTexEnvi gl_texture_env gl_operand2_rgb_arb gl_src_color

     
        glActiveTextureARB gl_texture0_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t2 ?ts
        GL.textureFunction $= GL.Replace

    let wrap m = do
        GL.renderPrimitive GL.Triangles m
        GL.alphaFunc $= Nothing
        glActiveTextureARB gl_texture1_arb
        GL.texture GL.Texture2D $= GL.Disabled
        glActiveTextureARB gl_texture0_arb
        GL.texture GL.Texture2D $= GL.Disabled
    let draw (Square _ s) = do
        let v = (0,s,0)
            b = (0,0,s*0.5)
            pt0 = (0,0,0)
            pt1 = pt0 `add` v
        draw_multi_tex_rect pt0 pt1 b
    wrap (mapM_ draw gs)


draw_prims (Branch (Flat s@(r,g,b,a)) _ _ _ _) gs = do
    GL.lighting $= GL.Enabled
    GL.texture GL.Texture2D $= GL.Disabled
    blending $= GL.Enabled
    GL.materialSpecular GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialDiffuse GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialAmbient GL.FrontAndBack $= GL.Color4 r g b a
    GL.materialEmission GL.FrontAndBack $= GL.Color4 0 0 0 0
    GL.materialShininess GL.FrontAndBack $= 0.1

    let draw (Branch _ w1 w2 p1@(px,py,pz) p2) = do
        let lv = p2 `sub` p1
            z_axis = (0, 0, 1)
            (angle,axis) = mk_rot_axis z_axis lv
            rot_axis = vec3 (norm (cross z_axis lv))
            rot_angle = r2d $ ang z_axis lv
        GL.translate (GL.Vector3 (px) (py) (pz))
        GL.rotate (r2d angle) (vec3 axis)
        let sxy = realToFrac w1
        with_push_mtx $ do
            GL.scale sxy sxy (absval lv)
            GL.callList ?l_cyl
        GL.translate (GL.Vector3 0 0 (absval lv) :: GL.Vector3 Float)
        GL.scale sxy sxy sxy
        GL.callList ?l_sph
    mapM_ (with_push_mtx.draw) gs

draw_prims (Pt _ _) gs = do
    GL.cullFace $= Nothing
    blending $= GL.Disabled
    GL.alphaFunc $= Just (GL.Greater, 0.5)
    GL.lighting $= GL.Disabled
    GL.texture GL.Texture2D $= GL.Enabled
    bind_texture "tuja" ?ts
    GL.textureFunction $= GL.Replace
    let draw (Pt pt0 v) = do
        draw_tex_rect_ori pt0 v 0.05
    GL.renderPrimitive GL.Triangles (mapM_ draw gs)
    GL.alphaFunc $= Nothing

draw_prims (PtA _ _ _ surf) gs = do
    GL.cullFace $= Nothing
    blending $= GL.Disabled
    GL.alphaFunc $= Just (GL.Greater, 0.5)
    GL.lighting $= GL.Disabled
    GL.texture GL.Texture2D $= GL.Enabled
    GL.alphaFunc $= Just (GL.Greater, 0.5)
    case surf of
     Tex tname -> do
        bind_texture tname ?ts
        GL.textureFunction $= GL.Replace

        let draw (PtA pt0 v size _) = do
            draw_tex_rect_ori pt0 v size
        GL.renderPrimitive GL.Triangles (mapM_ draw gs)
     TexBlend f t1 t2 -> do
        color (f,f,f,1)

        glActiveTextureARB gl_texture1_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t1 ?ts

        glTexEnvi gl_texture_env gl_texture_env_mode gl_combine_arb
        glTexEnvi gl_texture_env gl_combine_rgb_arb gl_interpolate_arb
        glTexEnvi gl_texture_env gl_source0_rgb_arb gl_previous_arb
        glTexEnvi gl_texture_env gl_operand0_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source1_rgb_arb gl_texture
        glTexEnvi gl_texture_env gl_operand1_rgb_arb gl_src_color
        glTexEnvi gl_texture_env gl_source2_rgb_arb gl_primary_color_arb
        glTexEnvi gl_texture_env gl_operand2_rgb_arb gl_src_color

     
        glActiveTextureARB gl_texture0_arb
        GL.texture GL.Texture2D $= GL.Enabled
        bind_texture t2 ?ts
        GL.textureFunction $= GL.Replace
        let wrap m = do
             GL.renderPrimitive GL.Triangles m
             GL.alphaFunc $= Nothing
             glActiveTextureARB gl_texture1_arb
             GL.texture GL.Texture2D $= GL.Disabled
             glActiveTextureARB gl_texture0_arb
             GL.texture GL.Texture2D $= GL.Disabled
        let draw (PtA pt0 v size _) = do
             draw_multi_tex_rect_ori pt0 v size
        wrap (mapM_ draw gs)
    GL.alphaFunc $= Nothing


draw_prims (LiA w1 w2 p1 p2 _ _) gs = do
    let f (LiA w1 w2 p1 p2 _ _) = Li w1 w2 p1 p2
    draw_prims (Li w1 w2 p1 p2) (map f gs)

draw_prims (Li _ _ _ _) gs = do
    GL.texture GL.Texture2D $= GL.Disabled
    blending $= GL.Disabled

    GL.lighting $= GL.Enabled
    GL.color (GL.Color3 0.6 0.4 0.2 :: GL.Color3 GL.GLfloat)
    let draw (Li a1 a2 p1@(px,py,pz) p2) = do
        let lv = p2 `sub` p1
            z_axis = (0, 0, 1)
            (angle,axis) = mk_rot_axis z_axis lv
            rot_axis = vec3 (norm (cross z_axis lv))
            rot_angle = r2d $ ang z_axis lv
        GL.translate (GL.Vector3 (px) (py) (pz))
        GL.rotate (r2d angle) (vec3 axis)
        GL.lighting $= GL.Enabled
        GL.texture GL.Texture2D $= GL.Disabled
        blending $= GL.Disabled
        GL.materialSpecular GL.FrontAndBack $= GL.Color4 0.15 0.15 0.0 1 
        GL.materialDiffuse GL.FrontAndBack $= GL.Color4 0.15 0.15 0.0 1
        GL.materialAmbient GL.FrontAndBack $= GL.Color4 0.15 0.15 0.0 1
        GL.materialEmission GL.FrontAndBack $= GL.Color4 0 0 0 0
        GL.materialShininess GL.FrontAndBack $= 0.1
        cylinder (age_to_rad a1) (age_to_rad a2) (realToFrac (absval lv)) 5 5
    mapM_ (with_push_mtx . draw) gs
 where
    age_to_rad::Age -> Double
    age_to_rad a = realToFrac (0.04 * a)



