module MetaLibLow where

import Helpers
import NonBasic
import Types
import PlantTypes
import LinAlg
import Misc (dbg, mapf, mapf2)
import Random

--------------------------------------------
-- Library ---------------------------------
--------------------------------------------

-- Distribution functions
-- - distributes power throughout the plant

dist_param me stdd gf blim x lc pos dev mdd = let dd f k = (*) f $ (stdd - dev + k)^3
                                                  e1 = if dev < stdd then (dd gf 0.1) else me
                                                  e4 p = (*) p $ if dev < stdd then (gf) else (me) 
                                                  ret =  case x of 
                                                        RootSelf -> e4 1.0
                                                        Self -> if mdd < dev then me else e1
                                                        Children f -> 1.0 -- if mdd < dev then (Just 0.0) else e4 f
                                              in ret

dist_simple me stdd x lc dev mdd = case x of
                                     Children y -> if dev < stdd then 1.0 else me
                                     _          -> 1.0
                                     
dist_shaped:: Float -> (Float -> Float) -> (Float,Float) -> DistFunc
dist_shaped str f (svel, mvel) = let g x lc pos dev mdd = let val = case x of
                                                                     Children y -> svel * (f y)
                                                                     _ -> mvel -- /(intToFloat (max 1 lc))
                                             in val
                        in (Df g str)

dist_shaped_im:: Float -> (Float -> Float -> Float) -> (Float,Float) -> DistFunc
dist_shaped_im str f (svel, mvel) = let g x lc pos dev mdd = let val = case x of
                                                                     Children y -> svel * (f y pos)
                                                                     _ -> mvel -- /(intToFloat (max 1 lc))
                                                         in val
                                    in (Df g str)


dist_shaped_w:: (Float -> Float) -> Float -> DistFunc
dist_shaped_w f w = let g x lc pos dev mdd = let val = case x of
                                                     Children y -> (f y) / (w * (max dev 1.0))
                                                     _ -> 1.0 / (w * (max dev 1.0))
                                         in val
                    in (Df g 0.0)
                                         
                                                       
                                                        

dist_root:: Float -> Float -> DistFunc
dist_root std vel = let g x lc pos dev mdd = if dev < std then vel else 0.0
                    in Df g 0.0

-- Direction functions                    
-- - angle from motherbranch

dirfnk::Vector -> Angledist
dirfnk v r f = rot_vec_axis v (0.0, 1.0, 0.0) (r * 6.28) 

-- Power Modification Functions
-- - Make adjustments according to growth constraints

-- mpfnc::ModDevFnc
-- mpfnc bs md indev = indev -- if limit then min (max (mdd - dev) 0.0) indev

mpfnc_id::ModDevFnc
mpfnc_id bs md indev = indev

mdfnc_param::Bool -> ModDevFnc
mdfnc_param limit bs md indev = let dev = bs_dev bs
			 	    mdd = md - (bs_pos bs)
				in if limit then min (max (mdd - dev) 0.0) indev
                                            else indev
                                              
mdfnc_limit::Float -> ModDevFnc
mdfnc_limit lim bs md indev = let dev = bs_dev bs
                                  v = lim - dev
                                  mv = (md / 3.0) - dev
                                  ret = max 0.0 (min mv indev)
                              in ret

mdfnc_prlen::Development -> Float -> ModDevFnc
mdfnc_prlen limit rlim bs md indev  = let dev = bs_dev bs
 	 		  	          pallowed = max (limit - dev) 0.0
                                          mdd = md - (bs_pos bs)
                                          rallowed = max ((md * rlim) - dev) 0.0
  			 	      in min pallowed $ min rallowed indev -- min indev md -- rallowed -- mallowed pallowed

mdfnc_rlen::Float -> ModDevFnc
mdfnc_rlen rlim bs md indev  = let dev = bs_dev bs
                                   mdd = md - (bs_pos bs)
                                   rallowed = max ((md * rlim) - dev) 0.0
  		                in min rallowed indev -- min indev md -- rallowed -- mallowed pallowed


mdfnc_mlen::ModDevFnc
mdfnc_mlen bs md indev  = let dev = bs_dev bs
			      mdd = md - (bs_pos bs)
			  in min (max (mdd - dev) 0.0) indev
                                              
mdfnc_plen::Development -> ModDevFnc
mdfnc_plen limit bs md indev  = let dev = bs_dev bs
			  	    allowed = max (limit - dev) 0.0
				in min indev allowed
                                              
mdfnc_mplen::Development -> ModDevFnc
mdfnc_mplen limit bs md indev  = let dev = bs_dev bs
 			  	     pallowed = max (limit - dev) 0.0
			             mdd = md - (bs_pos bs)
			             mallowed = min (max (mdd - dev) 0.0) indev
				     allowed = min allowed mallowed
  			 	 in min mallowed pallowed
				
mdfnc_mprlen::Development -> Float -> ModDevFnc
mdfnc_mprlen limit rlim bs md indev  = let dev = bs_dev bs
 	 		  	           pallowed = max (limit - dev) 0.0
                                           mdd = md - (bs_pos bs)
			                   mallowed = min (max (mdd - dev) 0.0) indev
                                           rallowed = max ((md * rlim) - dev) 0.0
  			 	       in min pallowed $ min mallowed $ min rallowed indev -- min indev md -- rallowed -- mallowed pallowed


-- Evolvement func
-- - To set time for next event

efnc::EvFnc
efnc rnd md = md + (rnd*0.1)

efnc_param::Development -> Float -> EvFnc
efnc_param dd sd rnd md = let dv = rnd*2.0*sd
                          in md + dd - sd + dv

                           
-- Branch helpers
-- - To create shapes
sinfunc::Point -> Point -> PosFunc
sinfunc (sx, sy, sz) (px, py, pz) bs param = (sx*(sin (param * px)), param , sz * (sin (param*pz)))  

-- Use curlfunc to create curlyness, typically old trees show curliness.
curlfunc::Point -> Point -> PosFunc
curlfunc (sx, sy, sz) (px, py, pz) bs param = f
                where f = let mp = min param px
                          in (sx * mp * (sin (param * px)), param, sz * mp * (cos (param * pz)))

-- Weighted Complex Branch, to combine Position functions.
wcomfunc::[PosFunc] -> PosFunc
wcomfunc ps bs p = mulk (1 / (intToFloat $ length ps)) (foldr add (0.0,0.0,0.0) $ mapf2 ps (repeat bs) (repeat p))


                                                                    
stdflower::PlantNode
stdflower = Fl [[]]

vcf::Int -> (Point->Point->PosFunc) -> (Float, Float) -> (Float, Float) -> [Float] -> [PosFunc]
vcf 0 _ _ _ _ = []
vcf n pfg (a,ma) (p,mp) (r0:r1:r2:r3:r4:r5:rs) = let f = pfg (a*r0, a*r1, a*r2) (p*r3, p*r5, p*r5)
                                                     fs = vcf (n-1) pfg (ma*a, ma) (mp*p, mp) rs
                                                 in (f:fs)

-- Modify position using modelspace position (instead of local)
modmspos::BranchState -> (Point -> Point) -> Point
modmspos bs f = let dir = bs_dir bs
                    a = ang dir dirup
                    q = case (abs a < 0.01, abs a > 3.13) of
                          (True,_) -> id_quat
                          (_,True) -> rot_axis (1,0,0) pi
                          _        -> rot_axis (norm (cross dir dirup)) a
                                       
                    r_imed = f dir
                    r = rot_vec_q r_imed q
                in r

std_color::Color
std_color = (0.5, 0.3, 0.3, 1.0)

std_bump::Point
std_bump = (0.5, 0.0, 0.0)

std_thick::Float
std_thick = 1.0

               
                                            
                    

