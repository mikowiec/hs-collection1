module Draw where

import NonBasic
import LinAlg
import GeomRep
import Types
import PlantTypes
import Helpers
import Misc (dbg)
import Monad
import MonadLib
--import BArrAtoms hiding (getmother)
import SimStateIHelpers

import List

-- draw _ _ = []
draw::Int -> SimStateI GeomRep
draw bsi = do
 --       m <- getmother
        glr <- draw2 (origo, id_quat) origo bsi 
        falleni <- getstp ist_fallen
        glrf <- draw_fallen falleni
   --     mprint $ (show $ (length glr) + (length glrf)) ++ " <- geom count"
        -- return (glr ++ glrf)
--        print "bah"
        return $ glr ++ glrf
            

mk_ival start step stop
 | start > stop = [stop]
 | otherwise    = start : mk_ival (start+step) step stop

get_sl :: SimStateI Float
get_sl = do
        s <- getstp ist_grpr
        return s

bsf::Int -> Float -> SimStateI Float
bsf i x = do 
               (bs::BranchState) <- getbranch i
               let rad = bs_rad bs
                   len = bs_dev bs
               let mf = rad -- <- mass_count i
               let s = 3.0
                   mxw = rad -- s * (sqrt len) / 5.0 * (sqrt $ sqrt mf)
                   lik = x / len < 2 / 3
                   mnw = mxw / 3
                   outer = 1 / 3 * len
                   inner = len - outer
                   k = if (outer == 0) then 0.0 else ((x - inner) /  outer) 
                   k' = 1.0 - k
               return $ (if lik then mxw else k' * mxw + k * mnw)

mk_d::Int -> SimStateI [(Development, Float)]
mk_d i = do
        bs <- getbranch i
        bss <- getbranches (bs_bs bs)
        let ps = map bs_pos bss
            ds = (f 0) $ map bs_dev bss
        rs <- mapM (`bsf` 0.0) (bs_bs bs)
        let r = reverse $ zip ps (f 0 rs)
        return r 

        where f m (x:xs) = let nm = max m x
                           in (nm:(f nm xs))

              f _ [] = []

justthick::Int -> Development -> SimStateI Development
justthick i d = do
        rs <- mk_d i
        let pr = f d rs        
        return $ pr

        where f d ((dev,rad):rs) = if d <= dev then rad else f d rs                    
              f d [] = 0

draw_fallen::[Int] -> SimStateI GeomRep
draw_fallen is = do
        reps <- mapM draw2_fallen is
        return $ concat reps

draw2_fallen::Int -> SimStateI GeomRep
draw2_fallen i = do
        bs <- getbranch i
        let q = rot_q_y (bs_dir bs)
            p = bs_pnt bs
        rep <- d2 (p, q) i
--        rep <- draw2 (p, q) origo i
        return rep

d2::(Point, Quat) -> Int -> SimStateI GeomRep
d2 (pos, quat) i = do
        bs <- getbranch i
        surf <- getsurface i Dead
        seed <- getstp ist_seed
        case (findnode (bs_pin bs) seed) of
                Just (Br sf emod _ _ _) -> do sl <- get_sl
                                              dev <- getbranchp i bs_dev 
                                              smrt <- mk_smr sl emod 0 dev 
                                              recl <- mapM (d3 i sf (pos, quat) emod) (bs_bs bs)
                                              ln <- mapM (mk_line emod) smrt -- (let xs = (mk_ival 0 sln dev) in zip xs (tail xs))
                                              return $ ln ++ (concat recl)
                
                Just (Le _ _ _ _ _ size) -> do case surf of
                                                Nothing -> return []
                                                Just s  -> return $ (PtA pos (bs_dir bs) size s):[]

      where                                  
            mk_smr lim emod a b = do
                 let stp = 2.0 
                 inv <- mk_smrt emod a stp stp lim
                 let minv = inv
                     uinv = min b inv
                 if a > b then do return []                             
                          else do invs <- mk_smr lim emod minv b
                                  return ((a,uinv):invs)


            mk_smrt emod a p s lim = do
                        apos <- trapos a emod
                        bpos <- trapos (a + (p*0.5)) emod
                        cpos <- trapos (a + p) emod
                        let abpos_half = bpos `sub` apos
                            acpos = cpos `sub` apos
                            abpos = 2.0 `mulk` abpos_half
                        let err = (absval $ abpos `sub` acpos)
                        if (err > lim) || (p > 1.0) then return (a + p)
                                                    else mk_smrt emod a (p + s) s lim


      
            mk_line emod (a,b) = do
                                 r0 <- r a -- max ((log $ age - a + 1)) mtt
                                 r1 <- r b -- max ((log $ age - b + 1)) mtt
                                 apos <- trapos a emod
                                 bpos <- trapos b emod
                                 return $ Li r0 r1 apos bpos -- (1.0,1.0,1.0,1.0) (1.0,1.0,1.0)
         
            endpos emod = do bsm <- getbranch i
                             fixpos (pos, quat) (bs_dev bsm) i
                         
            trapos p emod = do fixpos (pos, quat) p i

            r x = do 
                bs <- getbranch i 
                pr <- bsf i x 
                prr <- justthick i x 
                return $ max pr prr

            fixpos::(Point, Quat) -> Development -> Int -> SimStateI Point
            fixpos (p,q) dev bsi = do
                bs <- getbranch bsi
                seed <- getstp ist_seed                
                let p0 = getlocpos seed bs dev
                    nq = q 
                    p1 = p `add` rot_vec_q p0 nq
                return p1
                

d3::Int -> PosFunc -> (Point, Quat) -> EMod -> Int -> SimStateI GeomRep
d3 obsi sf (opos, oquat) emod bsi = do
        seed <- getstp ist_seed
        bs <- getbranch bsi
        bsm <- getbranch obsi
        
        let p0 = getlocpos seed bsm (bs_pos bs)
            p1 = opos `add` rot_vec_q p0 oquat
        
            nq = oquat `mulq` (rot_q_y $ bs_dir bs)
        
        d2 (p1, nq) bsi
 
getsurface::Int -> PhysStat -> SimStateI (Maybe Surface)
getsurface i ps = do 
           leaf <- getleaf i
           
           let ti = case ps of
                        Alive f -> 0
                        Dead    -> 1
           return $ case ps of 
                        Alive a -> Just $ TexBlend a (le_texfaded leaf) (le_texalive leaf)

                        Dead    -> Nothing 

draw2::(Point, Quat) -> Vector -> Int -> SimStateI GeomRep
draw2 (opos, oquat) pendpos i = do
        bs <- getbranch i      
        seed <- getstp ist_seed
        nq <- noquat
        alive <- getbranchp i bs_alive
        let dev = bs_dev bs    
        ret <- case (findnode (bs_pin bs) seed) of
                    Just (Br sf emod _ _ _) -> do epos <- endpos emod
                                                  sl <- get_sl
                                                  smrt <- mk_smr sl emod 0 dev 
--                                                  mprint $ (show $ ( (intToFloat $ length smrt) )) ++ " <- smrt"
                                                  recl <- mapM (draw3 i sf (opos, oquat) nq epos emod) (bs_bs bs)
                                                  ln <-  mapM (mk_line emod) smrt --(zip smrt (tail smrt)) -- (let xs = (mk_ival 0 sln dev) in zip xs (tail xs))
                                                  let ret = ln ++ (concat recl) -- (concatMap (draw3 i sf (opos, oquat) noquat (endpos emod) emod)  bs)
                                                  return ret

                    Just (Le _ _ _ _ _ size)   -> do let npos = getpos opos (0,0.0,0) nq
                                                     surf <- getsurface i alive
                                                     case surf of
                                                        Nothing -> return []
                                                        Just s  -> return $ (PtA npos (bs_dir bs) size s):[]

                    Nothing                  -> do return [] -- error "nej du, den gubben sket sig i skägget"
        return ret
                    
      where 
            mk_stp sl a b = do
                 let xs = let xs = (mk_ival a sl b) in zip xs (tail xs)
                 return xs

            mk_smr lim emod a b = do
                 let stp = lim
                 inv <- mk_smrt emod a stp stp lim
                 let minv = inv
                     uinv = min b inv
                 if a > b then do return []                             
                          else do invs <- mk_smr lim emod minv b
                                  return ((a,uinv):invs)


            mk_smrt emod a p s lim = do
                        apos <- trapos a emod
                        bpos <- trapos (a + (p*0.5)) emod
                        cpos <- trapos (a + p) emod
                        let abpos_half = bpos `sub` apos
                            acpos = cpos `sub` apos
                            abpos = 2.0 `mulk` abpos_half
                        let err = (absval $ abpos `sub` acpos)
                        if (err > lim) || (p > 1.0) then return (a + p)
                                                    else mk_smrt emod a (p + s) s lim

            
            ndir = do 
                nq <- noquat 
                return $ rot_vec_q (0,1,0) nq
           
            noquat = do
                bs <- getbranch i
                return $ mulq oquat (rot_q_y (bs_dir bs))
                
            mk_line emod (a,b) = do
                                 r0 <- r a -- max ((log $ age - a + 1)) mtt
                                 r1 <- r b -- max ((log $ age - b + 1)) mtt
                                 apos <- trapos a emod
                                 bpos <- trapos b emod
                                 return $ Li r0 r1 apos bpos -- (1.0,1.0,1.0,1.0) (1.0,1.0,1.0)
                                 
            endpos emod = do bsm <- getbranch i
                             grav2 i (opos, oquat) (bs_dev bsm)
            
            trapos p emod = do grav2 i (opos, oquat) p

            r x = do 
                bs <- getbranch i 
                pr <- bsf i x 
                prr <- justthick i x 
                return $ max pr prr

draw3::Int -> PosFunc -> (Point, Quat) -> Quat -> Point -> EMod -> Int -> SimStateI GeomRep
draw3 obsi sf (opos, oquat) noquat pendpos emod bsi = do
        seed <- getstp ist_seed
        bs <- getbranch bsi
        obs <- getbranch obsi
        new_pos <- grav2 obsi (opos, oquat) (bs_pos bs)
        draw2 (new_pos, noquat) pendpos bsi
        -- grav obs (rot_vec_q (sf obs (bs_pos bs)) noquat `add` opos) opos emod
 -- above line -> fix bs_pos regarding dev-age


