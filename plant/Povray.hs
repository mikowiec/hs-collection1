
module Povray where

import LinAlg
import Pf
import Types
import GeomRep
import Misc
import List

geomrep2povray pos ori rep = 
    geom2povray pos ori (map (LocGeom (0,0,0) id_quat (1,1,1) (-1)) rep)

geom2povray pos ori rep = 
 unlines
  [ "background { color rgb <0.3,0.3,0.3> }",
    "camera {",
    sprintf ("    location "`pv_vec`"") pos,
    sprintf ("    up "`pv_vec`"") ((0,1,0) `rot_vec_q` ori),
    sprintf ("    right "`pv_vec`"") ((-1,0,0) `rot_vec_q` ori),
    sprintf ("    look_at "`pv_vec`"") ((0,0,0.1) `rot_vec_q` ori),
    "    angle 4/3*80",
    "}",
    "",
    "global_settings { ",
    "    max_trace_level 50",
    "    adc_bailout 1/10",
    "}",
    "",
    "light_source {",
    "   <0, 20, 20> color rgb <1,1,1>",
    "}",
    ""] ++ def_textures rep ++ concatMap (concatMap show_geom . fix_blended pos) rep


fix_blended :: Vector -> LocGeom -> [LocGeom]
fix_blended cam lg@(LocGeom lp ori sc i rep) = lg : (filterJust $ map fix_blended' rep)
 where fix_blended' sq@(Square (TexBlend f _ s) sz) =
                    Just $ LocGeom (lp`add`v) ori sc i [Square (TexBlend (1-f) s s) sz]
        where v = 0.01 `mulk` (norm (cam `sub` lp))
       fix_blended' g = Nothing

t2l (x,y,z) = [x,y,z]

q2mat q = 
    let [c1,c2,c3] = map (`rot_vec_q` q) [(1,0,0),(0,1,0),(0,0,1)]
    in concat [t2l c1,t2l c2,t2l c3]

indent = unlines . map ("    "++) . lines

show_geom (LocGeom _ _ _ _ []) = ""
show_geom (LocGeom lp ori sc _ rep) = 
 let wrap c = case rep of [x] -> c ; _ -> "union {\n" ++ indent c ++ "}\n"
 in wrap $ concatMap (\r -> show_rep r ++ sprintf
    ("    scale "`pv_vec`"\n"++
     "    matrix "`pv_lst`"\n"++
     "}\n") sc (q2mat ori ++ t2l lp)) rep

is_blended_tex (TexBlend _ _ _) = True
is_blended_tex _ = False

tex_of_surf t@(Tex s) = [(s,t)]
tex_of_surf (TexBlend f s1 s2) = 
    let f_ = 1-f in [(g s1 f, TexBlend f s1 s2), (g s2 f_, TexBlend f_ s2 s1)]
 where g s f = (s ++ "_filter_" ++ map h (show f))
       h '.' = '_'
       h '-' = '_'
       h c = c
tex_of_surf _ = []

collect_textures rep = concatMap f rep
 where f (Square s _) = tex_of_surf s
       f (PtA _ _ _ s) = tex_of_surf s
       f _ = []

mk_tex_def (l,s) = 
 sprintf ("#declare "&." = "&." \n") l (indent $ surface_def s)

def_textures rep = 
    let ts = concatMap (collect_textures . (\(LocGeom _ _ _ _ r)->r)) rep
    in concatMap mk_tex_def ts

plane = 
 unlines 
  ["plane { <0, 1, 0>, 0", 
   "  finish {",
   "     ambient 1.0",
   "     phong 1.0",
   "  }",
   "  texture {",
   "    pigment { rgb <1.0,1.0,1.0> }",
   "  }",
   "}"]
 

show_rep (Li a1 a2 p1 p2) = cone p1 1 p2 1 (Flat (0,0.5,0,1))
show_rep (Pt p v) = sphere p (absval v) (Flat (0,0.5,0,1))
show_rep (Branch s w1 w2 p1 p2) = cone p1 w1 p2 w2 s
show_rep (Square s sz) = leaf (0,0,0) (0,sz,0) s
--show_rep (Surface _ _ _ _ _) = ""
show_rep (PtA p v r _) = sphere p r (Flat (0,0.5,0,1))
show_rep (FnBranch s pts (Fn sh)) =
    let xs = [ sh d | d <- fill_ival pts 0.1 ]
    in concatMap show_rep [ (Branch s w1 w2 p1 p2) | ((w1,p1),(w2,p2)) <- zip xs (tail xs) ]

sphere (x,y,z) r s = sprintf (
 "sphere {\n"++
 "    <"&","&","&">, "&"\n"++
 "    "&."\n") x y z r (surface s)


cone a _ b _ _ | absval (a `sub` b) == 0 = ""
cone (x1,y1,z1) w1 (x2,y2,z2) w2 s = sprintf (
 "cone {\n"++
 "    <"&","&","&">, "&", <"&","&","&">, "&"\n"++
 "       "&."\n") x1 y1 z1 w1 x2 y2 z2 w2 (surface s)


infixr 4 `pv_lst`
infixr 4 `uv`

pv_lst s a = s &> (("<"++).(++">").init.tail.show) <& a

uv s a = s &> sw <& a
 where sw (u,v) = sprintf ("<"&", "&">") u v

leaf pt0 v s = if absval v < 0.001 then "" else
    let b = v `cross` dirup
        b' = if absval b < 0.001 then (0,0,absval v) else b
        pt1 = pt0 `add` v
        p0 = pt0 `sub` b'
        p1 = pt1 `sub` b'
        p2 = pt1 `add` b'
        p3 = pt0 `add` b'
    in 
    sprintf (
        "mesh {\n"++
        "    triangle {\n"++
        "        "`pv_vec`", "`pv_vec`", "`pv_vec`" \n"++
        "        uv_vectors "`uv`", "`uv`", "`uv`"\n"++
        "    }\n"++
        "    triangle {\n"++
        "        "`pv_vec`", "`pv_vec`", "`pv_vec`" \n"++
        "        uv_vectors "`uv`", "`uv`", "`uv`"\n"++
        "    }\n"++
        "    uv_mapping\n"++
        "    "&."\n"++
        "    finish { ambient 0.8 }\n")
       p0 p1 p2 (0,1) (0,0) (1,0) p2 p3 p0 (1,0) (1,1) (0,1) (surface s)


surface :: Surface -> String 
surface (Flat (r,g,b,_)) =
    sprintf (
        "    texture {\n"++
        "       finish {\n"++
        "           ambient 0.4\n"++
        "           phong 1.0\n"++
        "       }\n"++
        "       pigment { rgb <"&","&","&"> }\n"++
        "    }\n") r g b

surface t =
    "texture { " ++ fst (head (tex_of_surf t)) ++ " }\n"

surface_def (Tex t) = 
    sprintf (
        "texture { \n"++
        "    pigment { image_map { png \"images/"&.".png\" } }\n"++
        "}\n") t

surface_def (TexBlend f t _) =
 sprintf (
    "texture {\n"++
    "    pigment { image_map { png \"images/"&.".png\" filter all "&" } }\n"++
    "}\n") t f



