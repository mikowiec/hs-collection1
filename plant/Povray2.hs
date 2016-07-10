
module Povray2 where

import LinAlg
import Pf
import Types
import GeomRep

geomrep2povray2 pos lookat ori rep =
 unlines
  [ "background { color rgb <1,1,1> }",
    "camera {",
    sprintf ("    location "`pv_vec`"") pos,
    sprintf ("    up "`pv_vec`"") ((0,1,0) `rot_vec_q` ori),
    sprintf ("    right "`pv_vec`"") ((-1,0,0) `rot_vec_q` ori),
    sprintf ("    look_at "`pv_vec`"") (lookat),
    "    angle 40",
    "}",
    "",
    "global_settings { ",
    "    max_trace_level 50",
    "    adc_bailout 1/10",
    "}",
    "",
    "light_source {",
    sprintf ("   "`pv_vec`" color rgb <1,1,1>") lpos,
    "}",
     "light_source {",
    sprintf ("   "`pv_vec`" color rgb <1,1,1>") pos,
    "}",
                                                 
    ""] ++ plane ++ concatMap show_rep2 rep

    where lpos = let (x,y,z) = pos
                 in (0.4*z, 2.0*y, 0.6*z)

show_rep2 (Li a1 a2 p1 p2) = cone p1 (a1*scal) p2 (a2*scal) (Flat (0.5,0.3,0.3,1))
        where scal = 0.032
show_rep2 (Pt p v) = sphere p (absval v) (Flat (0,0.5,0,1))
show_rep2 (Branch s w1 w2 p1 p2) = cone p1 w1 p2 w2 s
show_rep2 (PtA p v r sur) = leaf p (mulk r v) sur
        where lscal = 0.3


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
 

geomrep2povray pos ori rep =
 unlines
  [ "background { color rgb <1,1,1> }",
    "camera {",
    sprintf ("    location "`pv_vec`"") pos,
    sprintf ("    up "`pv_vec`"") ((0,1,0) `rot_vec_q` ori),
    sprintf ("    right "`pv_vec`"") ((-1,0,0) `rot_vec_q` ori),
    sprintf ("    look_at "`pv_vec`"") ((0,0,1) `rot_vec_q` ori),
    "    angle 4/3*80",
    "}",
    "",
    "global_settings { ",
    "    max_trace_level 50",
    "    adc_bailout 1/10",
    "}",
    "",
    "light_source {",
    "   <0, 0, -20> color rgb <1,1,1>",
    "}",
    ""] ++ concatMap show_rep rep

show_rep (Li a1 a2 p1 p2) = cone p1 0.01 p2 0.01 (Flat (0,0.5,0,1))
show_rep (Pt p v) = sphere p (absval v) (Flat (0,0.5,0,1))
show_rep (Branch s w1 w2 p1 p2) = cone p1 w1 p2 w2 s
show_rep (PtA p v r sur) = sphere p 0.01 (Flat (0,0.5,0,1))

sphere (x,y,z) r s = sprintf (
 "sphere {
    <"&","&","&">, "&"
       "&."
       finish {
           ambient 0.4
           phong 1.0
       }

    }\n") x y z r (surface s)


cone a _ b _ _ | absval (a `sub` b) == 0 = ""
cone (x1,y1,z1) w1 (x2,y2,z2) w2 s = sprintf (
 "cone {
    <"&","&","&">, "&", <"&","&","&">, "&"
     
       "&."
    }\n") x1 y1 z1 w1 x2 y2 z2 w2 (surface s)


infixr 8 `pv_vec`
infixr 8 `uv`
pv_showvec (x,y,z) = sprintf ("<"&", "&", "&">") x y z
pv_vec s a = s &> pv_showvec <& a

uv s a = s &> sw <& a
 where sw (u,v) = sprintf ("<"&", "&">") u v

leaf pt0 v (TexBlend a t0 t1) = if absval v < 0.01 then "" else
    let k = 1.0
        b = k `mulk` (v `cross` dirup)
        pt1 = pt0 `add` v
        p0 = pt0 `sub` b
        p1 = pt1 `sub` b
        p2 = pt1 `add` b
        p3 = pt0 `add` b
        surf = Tex $ if a < 0.5 then t0 else t1
    in 
    sprintf (
 "#declare tex = "&."
 mesh {
    triangle {
        "`pv_vec`", "`pv_vec`", "`pv_vec`" 
        uv_vectors "`uv`", "`uv`", "`uv`"
    }
    triangle {
        "`pv_vec`", "`pv_vec`", "`pv_vec`" 
        uv_vectors "`uv`", "`uv`", "`uv`"
    }
    uv_mapping
    texture { tex } 
    finish { ambient 0.8 }
 }\n") (surface surf) p0 p1 p2 (0,1) (0,0) (1,0) p2 p3 p0 (1,0) (1,1) (0,1)


surface (Flat (r,g,b,_)) =
    sprintf ("
    texture {
        pigment { rgb <"&","&","&"> }

        finish { ambient 0.4 phong 1.0 }


        normal { bumps 1.0   scale 0.02}
    }\n") r g b
surface (Tex id) = 
    sprintf ("
    texture { 
        pigment { image_map { png \"images/"&.".png\" } }
    }\n") id

