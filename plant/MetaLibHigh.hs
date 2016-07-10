module MetaLibHigh where

import MetaLibLow
import NonBasic
import Types
import PlantTypes
import LinAlg
import Misc (dbg, mapf, mapf2)
import Random
import Helpers

-- Interfaces

pineif::Interface
pineif = (IF (Df (\_ _ _ _ _->1.0) 0.0) 0 (dirfnk (0.7, 0.2, 0.0)) 0.2 ) efnc mpfnc_id

leafif::Float -> Interface
leafif dev = (IF (Df (\_ _ _ _ _->0.0) 0.0) 1 (dirfnk (0.7, 0.2, 0.0)) dev ) efnc mpfnc_id


strif::Interface
strif = (IF (Df (dist_param 0.1 1.0 1.0 0.0) 0.0) 0 (dirfnk (0.0, 1.0, 0.0)) 1.1 ) efnc mpfnc_id

angifi::Float -> Float -> Interface
angifi l u = let d_fnk = (\r f-> let l_rad = pi*l/180.0
                                     u_rad = pi*u/180.0
                                     y_dir = r*6.28
                                     x_dir = l_rad + ( (u_rad - l_rad) * r)
                                 in (rot_vec_axis (rot_vec_axis (0,1,0) (1,0,0) x_dir) (0,1,0) y_dir))
             in  IF (Df (\_ _ _ _ _->1.0) 0.0) 0 d_fnk 0.1 efnc mpfnc_id

angi::Float -> Float -> Angledist
angi l u r f= let l_rad = pi*l/180.0
                  u_rad = pi*u/180.0
                  y_dir = r*6.28
                  x_dir = l_rad + ( (u_rad - l_rad) * r)
              in (rot_vec_axis (rot_vec_axis (0,1,0) (1,0,0) x_dir) (0,1,0) y_dir)


angif::Float -> Float -> Interface
angif ang dev = let ang_rad = pi*ang/180.0
                    srate = 0.5
                in (IF (Df (dist_param 0.1 1.0 1.0 0.0) 0.0) 2 (dirfnk (rot_vec_axis (0,1,0) (1,0,0) ang_rad) ) dev) efnc mpfnc_id



-- Branches

sinbranch::Point -> Point -> EMod -> PlantNode
sinbranch (sx,sy,sz) (px,py,pz) emod = Br (\_ param -> (sx*(sin (param * px)), param , sz * (sin (param*pz)))) emod std_color std_bump std_thick


combranch::[PosFunc] -> EMod -> PlantNode
combranch ps e = let len = length ps
                     f bs p = foldr add (0.0,0.0,0.0) $ mapf2 ps (repeat bs) (repeat p)
                 in Br f e std_color std_bump std_thick

cmxbranch::EMod -> PlantNode                       
cmxbranch e = combranch [\_ p -> (p/2.0, 5.0 * sin (p / 5.0),  0.0)]  e

strbranch::PlantNode
strbranch = Br (\_ p -> (0,p,0)) 1.0 std_color std_bump std_thick


wigbranch::Float -> Float -> EMod -> PlantNode
wigbranch s p emod = sinbranch (s,0,s) (p,0,p*0.3) emod

curlbranch p emod = Br f emod std_color std_bump std_thick      
        where f bs param = let mp = min param 0.02
                               ps = 10
                           in (mp * (sin (param * ps)), 0.2 * param, mp * (cos (param * ps)))

comwigbranch::Float -> Float -> Float -> PlantNode
comwigbranch s p emod = Br f emod std_color std_bump std_thick      
        where f bs param = let f0  = br_posfnc $ sinbranch (s, 0, s) (p, 0, p*0.5) 0.0
                               f1  = br_posfnc $ sinbranch (s*2, s*2, s*2) (p*1/2, p*2/2, p*1/2) 0.0
                               f2  = br_posfnc $ sinbranch (s*3, s*3, s*3) (p*1/3, p*2/3, p*1/3) 0.0
                               f3  = br_posfnc $ sinbranch (s*4, s*4, s*4) (p*1/4, p*2/4, p*1/4) 0.0
                               f4  = br_posfnc $ sinbranch (s*10, s*10, s*10) (p*1/10, p*2/10, p*1/10) 0.0
                               f5  = br_posfnc $ sinbranch (s*50, s*50, s*50) (p*1/50, p*2/40, p*1/50) 0.0
                           in (wcomfunc [f0, f1, f2, f3,f4]) bs param

rootbranch::Float -> EMod -> PlantNode
rootbranch p emod = Br f emod std_color std_bump std_thick
        where f bs param = let curlfocus = min ((bs_dev bs) / 5.0) 1.0
                               f0 b p = mulk curlfocus ( (br_posfnc $ curlbranch 1.0 0.0) b p)
                               f1 b p = mulk (1.0 - curlfocus) ( (br_posfnc strbranch) b p)
                           in (wcomfunc [f0, f1]) bs param 

upbranch::EMod -> PlantNode
upbranch emod = Br f emod std_color std_bump std_thick
                where f bs param = let scale = 1.0 / (bs_dev bs)
                                       y_corr = (param / 3.0) * ((param * scale) ^ 2)
                                       f p = let (x,y,z) = mulk param p
                                             in (x, y + y_corr, z)
                                   in modmspos bs f


ranwigbranch::Float -> EMod -> PlantNode
ranwigbranch amp emod = Br f emod std_color std_bump std_thick
                 where f bs p = let rs = rndl (0.3,1) (bs_rnd bs) 
                                in wcomfunc (vcf 6 sinfunc (amp*1.0, amp*0.7) (0.9, 2.0) rs) bs p

ranagebranch::Float -> EMod -> PlantNode
ranagebranch amp emod = Br f emod std_color std_bump std_thick
        where f bs p = let rs = rndl (0.2, 1) (bs_rnd bs)
                           camp = (log (bs_age bs) )
                           cs =  wcomfunc (vcf 5 sinfunc (camp*0.1, 0.7) (1.0, 2.0) rs) 
                           cc =  wcomfunc (vcf 5 curlfunc (camp*0.4, 0.6) (0.7, 2.0) rs) 
                       in (wcomfunc [cs, cc]) bs p


rotbranch e = combranch [\_ p -> mulk (sin 0) (sin p, 0.0, cos p)] e 
-- Leaves

stdleaf::Float -> Size -> PlantNode
stdleaf lim size = Le [] 0.01 survival "leaf1" "leaf1faded" size
    where survival md pos = (md - pos) < lim

pineleaf::Float -> Size -> PlantNode
pineleaf lim size = Le [] 0.01 survival "tuja" "tuja" size
    where survival md pos = (md - pos) < lim
    
-- Root functions 

rootfnc :: RootFunc
rootfnc rad rsc = let r = rad + 0.1
                      pow = (f "water" rsc)
                  in r * pow 
     where f res [] = 0.0
           f res ((x,v):xs) = if x == res then v
                                          else f res xs
                                          
pinedirfnc::Angledist
pinedirfnc r f@(x,y,z) = let a = (x,y,z)
                             qy = dirfnk (0.2,0.8,0.2) r f
                             
                             ret = if r < 0.5 then cross qy a
                                              else cross (mulk (-1) qy) a
                             ret_alt = angi 70 100 r f
                             y_aligned = (absval ret) < 0.01
                         in if y_aligned then ret_alt else ret 

 
