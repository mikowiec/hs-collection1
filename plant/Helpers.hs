module Helpers where

import Types
import PlantTypes
import NonBasic
import LinAlg
import Random


-- Specific                 

rndl::(Float, Float) -> Rnd -> [Float]

rndl (l, u) r = let (rnd, r') = randomR (l, u) r
                in rnd:(rndl (l, u) r)
--rndl (l,u) _ = cycle [l,l+((u-l)/10)..u]

limMayDev::Development -> Maybe Development -> Development
limMayDev dev Nothing = dev
limMayDev dev (Just x) = min dev x
   
intToFloat:: Int -> Float
intToFloat n = fromInteger $ toInteger n
                         
get_disfunc::Seed -> PlantId -> BranchDist
get_disfunc _ [] = Df  (\_ _ _ _ _-> 1.0) 0.0
get_disfunc s pin = let (Cr inf _ _) = findcrown pin s
                    in (if_bdist inf)
                        
get_moddevfnc::PlantId -> Seed -> ModDevFnc
get_moddevfnc [] _ = error "get_moddevfunc got empty pin"
get_moddevfnc pin s = let (Cr inf _ _) = findcrown pin s
                      in (if_moddev inf)
                
findcrown::PlantId -> Seed -> Crown
findcrown [] _ = error "findcrown: gotta give me a non-empty id..."
findcrown (p:ps) (Se _ _ cs) = f ps (cs!!p)
    where f::PlantId -> Crown -> Crown
          f [] c = c
          f (p:ps) (Cr _ _ cs) = f ps (cs!!p)

isValidPin::PlantId -> Seed -> Bool
isValidPin [] _ = False
isValidPin pin (Se _ _ (cs:_)) = let cd = d cs
                                     pl = length pin
                                 in pl <= cd    
          where   d::Crown -> Int
                  d c = d' 1 c
                  d' n (Cr _ _ []) = n
                  d' n (Cr _ _ cs) = d' (n + 1) (head cs)
                                                        
findnode::PlantId -> Seed -> Maybe PlantNode
findnode [] _ = Nothing
findnode pin seed = Just $ f (findcrown pin seed)
    where f (Cr _ pn _) = pn

isLeafBs::BranchState -> Bool
isLeafBs bs = case bs_mode bs of
                Branch -> False
                Leaf   -> True

isLeaf::PlantId -> Seed -> Bool
isLeaf pin s = case findnode pin s of
                Just (Le _ _ _ _ _ _) -> True
                _                   -> False       
    
findleaf::PlantId -> Seed -> PlantId
findleaf [] seed = findleaf [0] seed
findleaf pin seed = pin ++ (f (findcrown pin seed))
    where f c@(Cr _ _ []) = []
          f c@(Cr _ _ (cs)) = magic:(f (cs!!magic))
          magic = 0

findif::PlantId -> Seed -> Interface
findif pin seed = let (Cr inf _ _) = findcrown pin seed
                  in inf
          
dirfunc::Float -> Vector -> Crown -> Direction
dirfunc rnd fwd (Cr inf _ _) = (if_adist inf) rnd fwd

{-
bs_depth:: BranchState -> Int
bs_depth bs = 1 + bs_depth' (bs_bs bs)
 where bs_depth' [] = 0
       bs_depth' xs = maximum (map bs_depth xs)
-}
           
{-      
getMSPos::BranchState -> PlantId -> Seed -> Development -> Point -> Quat -> Point
getMSPos bs pin seed dev opos q =
    case (findnode pin seed) of
        Just (Br sf emod) -> let lpos = getlocpos seed bs dev 
                                 epos = getpos opos lpos q
                                 gpos = grav2 seed bs (opos, q) dev-- grav bs epos opos emod
                             in gpos
                             
        Just (Le _ _ _ _ _) -> let lpos = (0,0.15,0)
                                   epos = getpos opos (0,0,0) q
                                   gpos = epos -- grav bs epos opos 1.0
                               in gpos
                             
        Nothing           -> origo                             
    
        where dir = rot_vec_q (0,1,0) q
              age = bs_age bs
-} 
 
getpos::Point -> Point -> Quat -> Point
getpos opos npos noquat = rot_vec_q npos noquat `add` opos
 -- rot npos dirup nodir `add` opos

getlocpos::Seed -> BranchState -> Development -> Point
getlocpos seed bs dev = let pin = bs_pin bs
                            (Just (Br posfunc _ _ _ _)) = findnode pin seed
                            (x,y,z) = posfunc bs dev
                        in (x,y,z)

getemod::PlantId -> Seed -> EMod
getemod pin seed = case findnode pin seed of
                    Just x -> br_emod x
                    _ -> error "Helpers.getemod called unwisely"                    


