module MetaPlants where

import Meta
import Helpers
import NonBasic
import Types
import PlantTypes
import LinAlg
import Misc (dbg, mapf, mapf2)
import MetaLibHigh
import MetaLibLow
---------------------------------------
-- Examples ---------------------------
---------------------------------------

tt = expr 

com::Common
com = Co

expr::Seed
expr = ((Ea 1.0 1 rootfnc 55.0), com)

        .<< (if_setdist (dist_root 3.3 4.1) $ strif, 
             br_setthick 0.1 $ wigbranch 1.0 0.0 0.0) 
             
         << (if_setspwn 8 $ if_setmod (mdfnc_mprlen 5.0 0.4)  
                          $ if_setevol (efnc_param 0.35 0.1) 
                          $ if_setdist (dist_shaped 1.4 f_tri (1.1, 0.1) ) 
                          $ angifi 90 90 , 
             br_setthick 0.1 $ upbranch 0.2 ) 
 
         << (if_setmod (mdfnc_mprlen 0.5 0.3 )  $ if_setdir pinedirfnc 
                                                $ if_setevol (efnc_param 0.05 0.005) 
                                                $ if_setdist (dist_shaped 0.3 f_str (1.0,1.0)) 
                                                $ angifi 35 55 , 
             br_setthick 0.1 $ wigbranch 1.0 0.0 0.3) 
          
         <<.(if_setevol (efnc_param 0.05 0.0) $ if_setdir pinedirfnc 
                                              $ leafif 0.05, 
             pineleaf 4.0 0.1) 
	   
         where f0 x dev = (abs $ (cos (dev * pi/6) )) + 0.3
               f_sin x = cos (x * pi/2)
               f_tri x = (1.0 - x) + 0.1
               f_str x = 1.0
               f_con x = if x < 0.66 then 1.0 else ((1.0 - x) / 0.66)
 
betula::Seed
betula = ((Ea 1.0 1 rootfnc 10.0), com)

        .<< (if_setdist (dist_root 10.0 29.1) $ strif, 
             br_setthick 0.1 $ ranagebranch 0.3 0.0) 
             
         << (if_setmod (mdfnc_rlen 0.7)  $ if_setevol (efnc_param 0.5 0.2) $ if_setdist (dist_shaped 1.4 f0 (16.0, 0.1) ) $ angifi 20 45 , 
             br_setthick 0.1 $ ranwigbranch 0.5 0.2) 

         << (if_setmod (mdfnc_mplen 0.7)  $ if_setevol (efnc_param 0.14 0.01) $ if_setdist (dist_shaped 0.3 f0 (7.0,1.0)) $ angifi 35 55 , 
             br_setthick 0.1 $ ranwigbranch 0.5 0.4) 
             
         << (if_setmod (mdfnc_mplen 0.1 ) $ if_setevol (efnc_param 0.05 0.005) $ if_setdist (dist_shaped 0.3 f0 (3.0,1.0)) $ angifi 45 65, 
             br_setthick 0.1 $ ranwigbranch 0.5 0.9) 
{-
         << (if_setmod (mdfnc_plen 0.2) $ if_setevol (efnc_param 0.05 0.03) $ if_setdist (dist_shaped 0.3 f0 (0.1,1.0)) $ angifi 30 120, 
            wigbranch 1.0 0.3 0.95) 
       -}      
         <<.(if_setevol (efnc_param 0.05 0.0) $ leafif 0.05, 
            stdleaf 4.0 0.10) 
	   
         where f0 x = 1 -- cos (pi/2.0 * x) 
               f_tri x = 1.0 - x
               f_con x = if x < 0.66 then 1.0 else ((1.0 - x) / 0.66)
    
pine::Seed
pine = ((Ea 2.0 1 rootfnc 1.0), com)

            .<< (if_setdist (dist_root 5.0 1.0) $ strif,
                 wigbranch 0.1 0.0 0.0)
              
             << (if_setevol (efnc_param 0.02 0.005) $ if_setdist (dist_shaped 0.2 f_pine (0.2, 0.2)) $ angif 120 0.0,
                 upbranch 0.1)              
                                                  
             <<. (if_setmod mdfnc_mlen $ if_setdir pinedirfnc $ if_setevol (efnc_param 0.01 0.002) $ if_setdist (dist_shaped 0.05 f_str (1.0,1.0)) $ angif 90 0.0,
                 wigbranch 0.1 0.0 0.2)
                 
          --  <<.(if_setspwn 1 $ leafif 0.001,
          --       stdleaf 4.0 0.05)
            where f_str x = 1.0
                  f_tri x = 1.0 - x
                  f_pine x = if x < 0.8 then 1.0 else ( (1.0 - x)/ 0.2)
            
{-            
exper::Seed
exper = (Ea 2.0 1 rootfnc 1.0)

            .<< (if_setdist (dist_param 0.1 1.0 1.0 0.0) $ strif,
                 cmxbranch 1.0)
              
             << (if_setevol (efnc_param 0.9 0.0) $ if_setdist (dist_simple 0.1 4.8) $ angif 100 0.0,
                 wigbranch 0.6 0.8)              
                                                  
            << (if_setdir pinedirfnc $ if_setevol (efnc_param 0.05 0.0) $ if_setdist (dist_simple 0.1 2.0) $ angif 90 0.0,
                wigbranch 0.0 0.6)
                 
          --   << (if_setdir pinedirfnc $ if_setevol (evfnc 1.0 0.0) $ if_setdist (dist_param 0.0 0.1 0.5 0.0) $ angif 80 0.0,
          --       wigbranch 0.3 0.7)

             <<.(if_setspwn 1 $ leafif 0.065,
                 stdleaf 6.0)

  -}               
{-
columnar::Seed
columnar = (Ea 1.0 1 rootfnc 3.0) 

        .<< (if_setdist (dist_param 0.03 5.0 0.5 0.3) $ strif, 
             wigbranch 0.0 1.0) 
             
         << (if_setevol (evfnc 0.0 0.1) $ if_setdist (dist_param 0.1 5.0 0.5 0.5) $ angif 30 0.0 , 
             wigbranch 0.0 0.9) 
             
         << (if_setevol (evfnc 0.05 2.0) $ if_setdist (dist_param 1.2 3.0 0.5 0.1) $ angif 60 1.0 , 
             wigbranch 0.0 0.8) 
             
 --        << (if_setevol (evfnc 0.1 4.0 ) $ if_setdist (dist_param 0.3 2.0 1.0 0.0) $ angif 45 0.5, 
   --          wigbranch 0.0 0.4) 
             
         <<.(leafif 0.2, 
             stdleaf 4.0) 
                 
-}
