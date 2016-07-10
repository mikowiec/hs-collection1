
module Draw2 where


import PlantTypes hiding (Branch, Leaf)
import Monad
import GeomRep as G
import Seed2 as S
import Simul2
import LinAlg
import Types
import Misc
import MonadLib
import Maybe
import FiniteMap
import ApproxTree
import List

draw :: MainStruct -> GNode
draw (ix, smap) = Param $ Fn $ \(_,g) -> draw2 smap origo id_quat g ix

find_dst :: StructMap -> StructID -> Structure
find_dst sm i = maybe (error "Draw2.find_dst") id (ApproxTree.find sm i)

approx_ori sh p =
    let v = snd (sh (p+0.01)) `sub` snd (sh (p-0.01))
    in rot_q_y v

draw2 sm lo ori g ix = 
     Offset lo $ Ori ori $ Group $ (Tag ix geom) : gs
 where
        g' = g `rot_vec_q` (inv ori)
        Structure (Part code) _ cs = find_dst sm ix
        pos = get_c_pos code
        shape = get_c_shape code
        geom = Geometry $ (get_c_geom code) g' pos
        gs = map cld_fn cs
        cld_fn (p,o,i) =
            let (w,lp) = shape g' p
                o' = approx_ori (shape g') p
                cdir = dirup `rot_vec_q` (o `mulq` o')
            -- Put the next structure on the surface, not centered..
                out_vec = w `mulk` ((cdir `sub` (0,(cdir `dot` dirup),0)))
            in draw2 sm (lp`add`out_vec) (o `mulq` o') g' i



-- ids are never reused...

draw_interpolated :: MainStruct -> MainStruct -> GNode
draw_interpolated (ix,smap) (ix',smap') =
    draw2i smap smap' origo id_quat ix

wavg p v1 v2 = (1-p)*v1 + p*v2
wavg_vec p v1 v2 = ((1-p) `mulk` v1) `add` (p `mulk` v2)

wavg_quat p q q' = slerp q q' p

combine_codes p code Nothing = (get_c_pos code, get_c_shape code)
combine_codes p code (Just code') = (npos,nsh)
 where npos = wavg p (get_c_pos code) (get_c_pos code')
       (sh,sh') = (get_c_shape code, get_c_shape code')
       nsh = \g p -> let (w,v) = sh g p
                         (w',v') = sh' g p
                     in (wavg p w w', wavg_vec p v v')

draw2i sm sm' lo ori ix =
     Offset lo $ Ori ori $ Param $ Fn $ \p -> Group $ g p
 where
  g (p,gv) = Tag ix geom : (gs)
   where
    Structure (Part code) _ cs = find_dst sm ix
    child = fmap (\(Structure (Part code) _ cs) -> (code)) (ApproxTree.find sm' ix)
    (pos,shape) = combine_codes p code child
    geom = Geometry $ (get_c_geom code) gv pos
    gs = map cld_fn cs
    cld_fn (p,o,i) =
        let (w,lp) = shape gv p
            o' = approx_ori (shape gv) p
            cdir = dirup `rot_vec_q` (o `mulq` o')
            out_vec = w `mulk` ((cdir `sub` (0,(cdir `dot` dirup),0)))
        in draw2i sm sm' (lp`add`out_vec) (o `mulq` o') i


