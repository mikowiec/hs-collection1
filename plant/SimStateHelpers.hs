module SimStateHelpers where

import NonBasic
import PlantTypes
import MonadLib
import Monad
import Types
import Helpers
import Misc
import LinAlg
import List
import Random
import IOExts
import Array
import NonBasic
import BArrAtoms

import Array
import ST
import Data.Array.MArray
import Data.Array.ST
import STArrLib

clone::Int -> SimState s ()
clone i = do
        bs <- getbranch i
        rndgen <- getrndgen
        modbranch i (\bs -> bs {bs_rnd = rndgen})
        clone_dir i
        mapM_ clone (bs_bs bs)

clone_dir::Int -> SimState s ()
clone_dir i = do
        bs <- getbranch i
        ran <- getran
        let nang = ran * 360.0
            new_dir = rot_vec_axis (bs_dir bs) (0.0, 1.0, 0.0) nang
        modbranch i (\bs -> bs {bs_dir = new_dir})

collectstats::Int -> SimState s Float
collectstats i = do
        bs <- getbranch i
        is_leaf <- isLeafBsi i
        rlcnt <- mapM collectstats (bs_bs bs)
        let nlcnt = if is_leaf then (bs_lcnt bs)
                               else (sum rlcnt)
        modbranch i (\bs -> bs {bs_lcnt = nlcnt})
        return nlcnt

printmother t = do 
        mum <- getmother
        cbs <- getstp st_cbs
        mprint $ (show cbs) ++ " <- " ++ t

printpow info = do 
        st <- getst
        bsm <- getmother
        power <- getpowout
        time <- gettime
        mprint $ (show (power, bs_pin bsm, bs_min bsm, time)) ++ " <- power, time " ++ info
        return ()


--------------------------
freezeState::State s -> TreeState s IState
freezeState st = do
        let seed = st_seed st -- <- getstp (st_seed.tree_state)
            env = st_env st -- <- getstp (st_env.tree_state)
            cbs = st_cbs st -- <- getstp (st_cbs.tree_state)
            (marr::STArray s Int BranchState) = (barr_arr.st_barr) st
        farr <- lift (do (farr::Array Int BranchState) <-  unsafeFreeze marr
                         return farr)
        let ibarr = IBArr farr ( (barr_max.st_barr) st) ( (barr_curr.st_barr) st)
            istate = ISt (st_rnd st) seed env (st_idcnt st) (st_powacc st) (st_flags st) (st_fallen st) (st_fstack st) ibarr cbs (st_grpr st)
        return istate
       
thawState::IState -> TreeState s (State s)
thawState ist = do
        marr <- lift (do (marr::STArray s Int BranchState) <- thawarr ((ibarr_arr.ist_barr) ist)
                         return marr)
        st <- istateToState ist marr
        return st

istateToState::IState -> STArray s Int BranchState -> TreeState s (State s)
istateToState ist marr = do
        let barr = BArr marr ((ibarr_max.ist_barr) ist) ((ibarr_curr.ist_barr) ist)
            pos = (origo, id_quat)
            md = 0.0
            powout = 0.0
            powback = Left 0.0
            time = 0.0
            powacc = ist_powacc ist 
            st = St (ist_rnd ist) (ist_seed ist) (ist_env ist) pos md powout powback time "" (ist_idcnt ist) powacc (ist_flags ist) (ist_fallen ist) (ist_fstack ist) barr (ist_cbs ist) [] (ist_grpr ist)
        return st
        
            
thawarr arr = (unsafeThaw arr) >>= \(marr::STArray s Int BranchState) -> return marr 

letthemfall::[Int] -> SimState s ()
letthemfall is = do
        modbranches is groundcoords 
        fallen <- getstp st_fallen
        modst (\st -> st {st_fallen = fallen ++ is})

        where groundcoords bs = let (x,y,z) = bs_pnt bs
                                    (dx, dy, dz) = bs_dir bs
                                    npnt =  (x, 0, z)
                                    ndir =  norm (dx, 0, dz)
                                in bs {bs_pnt = npnt, bs_dir = ndir}
                
partBsI::[Int] -> SimState s [[Int]]
partBsI is = do
        bsm <- getmother
        let l0 = (length $ bs_pin bsm) + 1
        lpin <- getlpin
--        error $ (show (lpin, length lpin, bs_pin bsm)) ++ "pfft" 
        let l1 = (length lpin) - 1
 --       error $ (show (is, l0, l1)) ++ " <- part"
        pl <- mapM f [l0 .. l1]
        return pl
        
        where f n = do
                tmp <- filterBsI (\bs -> n == (length $ bs_pin bs)) is
                return tmp

filterBsI::(BranchState -> Bool) -> [Int] -> SimState s [Int]
filterBsI f is = do
        bss <- getbranches is
        let bst = zip is bss
        return $ map fst $ filter (\(_,bs) -> f bs) bst

mapBsI::(BranchState -> a) -> [Int] -> SimState s [a]
mapBsI f is = do
        bss <- getbranches is
        return $ map f bss

grav2::Int -> ModelPos -> Development -> SimState s Point
grav2 i (p,q) dev = do
        bs <- getbranch i
        seed <- getstp st_seed
        mass <- mass_count i
        let p0 = getlocpos seed bs dev
            qf = q `mulq` rot_q_y (bs_dir bs)
            p1@(x,y,z) = p `add` rot_vec_q p0 qf
            emod = getemod (bs_pin bs) seed
            param0 = mass * emod * (absval p0)
            paramx = if ( (bs_dev bs) == 0) then 0 else dev / (bs_dev bs)
            gforce = param0 * paramx
            pret = (x, y - gforce, z)
        return pret

isLeafBsi::Int -> SimState s Bool
isLeafBsi i = do
        bs <- getbranch i
        return (isLeafBs bs)

isGrowBsi::Int -> SimState s Bool
isGrowBsi i = do
        bs <- getbranch i
        return $ case (bs_last bs) of
                        Just x -> x /= 0
                        Nothing-> True

isBranchBsi::Int -> SimState s Bool
isBranchBsi i = do
        bs <- getbranch i
        return (not $ isLeafBs bs)


leaf_count :: Int -> SimState s Int
leaf_count i = do
        bs <- getbranch i
        recl <- mapM leaf_count (bs_bs bs)
        let recs = sum recl
            lr = if (isLeafBs bs) then 1 else 0
        return (lr + recs) 
                     
mass_count :: Int -> SimState s Float
mass_count i = do
        bs <- getbranch i
        recl <- mapM mass_count (bs_bs bs)
        let recs = sum recl
            lr = if (isLeafBs bs) then 0.0 else (bs_dev bs)
        return (lr + recs) 

getMSPos::Int -> Int -> SimState s Point
getMSPos mi ci = do
        seed <- getstp st_seed
        bs <- getbranch ci
        q <- getnewq
        let dev = bs_pos bs
        opos <- getstp (fst.st_pos)
        case (findnode (bs_pin bs) seed) of
                Just (Br sf emod _ _ _) -> do 
                                        --let lpos = getlocpos seed bsm dev 
                                        gpos <- grav2 mi (opos, q) dev-- grav bs epos opos emod
                                        return gpos
                          
                Just (Le _ _ _ _ _ _) -> do let lpos = (0,0,0)
                                                epos = Helpers.getpos opos (0,0,0) q
                                                gpos = epos -- grav bs epos opos 1.0
                                            return gpos
                                     
                Nothing           -> return origo                             
 
getleaffood::Int -> SimState s Float
getleaffood i = do
        seed <- getstp st_seed
        bs <- getbranch i
        let (Just leaf) = findnode (bs_pin bs) seed
        return $ le_cons leaf


--------------------------

pushf::Float -> SimState s ()
pushf val = do st <- getst
               let stk = st_fstack st
               modst (\st -> st {st_fstack = val:stk})
               return ()
              
popf::SimState s Float
popf = do st <- getst
          let stk = st_fstack st
          modst (\st -> st {st_fstack = tail stk})
          return (head stk)
          
pushvitals::SimState s ()
pushvitals = do
        cbs <- getstp st_cbs
        md <- getstp st_md
        cvit <- getstp st_vit
        let nv = Vi cbs md
        modst (\st -> st {st_vit = (nv:cvit)})
        
popvitals::SimState s ()
popvitals = do
        (v:vs) <- getstp st_vit
        modst (\st -> st {st_vit = vs, st_cbs = vi_cbs v, st_md = vi_md v})

readflag::Flags -> SimState s Bool
readflag f = do st <- getst
                let c = (st_flags st)!f
                return c

writeflag::Flags -> Bool -> SimState s ()
writeflag f param = do st <- getst
                       let oflags = st_flags st
                           nflags = oflags//[(f,param)]
                       modst (\st -> st {st_flags = nflags})
              
normlist xs = let sum = foldr (+) 0 xs
                  xs' = if sum > 0.0 then map (\x -> x / sum) xs else xs
              in xs'
              
limitlist _ [] = []              
limitlist lim (x:xs) = if (x > lim) then (lim: (take (length xs) (repeat 0)))
                                    else (x: (limitlist (lim - x) xs))
              
negtozerolist xs = map (\x -> if x < 0 then 0 else x) xs              

mprint d = getst >>= (\gst -> (setst (trace (reverse $ reverse $ d) gst)))

printstats::SimState s ()
printstats = do
        arrsize <- getstp (barr_max.st_barr)
        bsm <- getmother
        pow <- getstp st_powout
        mprint $ (show pow) ++ " / " ++ (show arrsize) ++ " / " ++ (show $ bs_dev bsm) ++ " / " ++ (show $ bs_age bsm) ++  " <= pow / mem / dev / age"
        cbs <- getstp st_cbs
        when (not $ null cbs) (do bc <- branch_count $ head cbs
                                  lc <- leaf_count $ head cbs
                                  mprint $ (show bc) ++ " / " ++ (show lc) ++ " <= branches / leaves ")

getgrow::SimState s Float
getgrow = do st <- getst
             let earth = se_earth (st_seed st)
             return (ea_boost earth)


sendback::Power -> Power -> SimState s ()
sendback p a = do   mydist <- my_dist
                    st <- getst
                    -- let (ret, acc) = if mydist == 0.0 then (0.0, p) else (p, 0.0)
                    let newpowacc = (st_powacc st) + a
                    modst (\st -> st {st_powout = p, st_powacc = newpowacc })            
{-               
addbss::State s -> SimState s ()
addbss ost = do st <- getst
                let bss = st_bs ost
                    bs = head $ st_bs st
                modst (\st -> st {st_bs = (bs:bss)})
                return ()
-}
get_interfacep::PlantId -> (Interface -> a) -> SimState s a
get_interfacep pin f = do
        inf <- get_interface pin
        return $ f inf 


get_interface::PlantId -> SimState s Interface
get_interface pin = do
        seed <- getstp st_seed
        let inf = findif pin seed
        return inf

bdli::[Int] -> SimState s [Float]
bdli is = do
        bcl <- mapM branch_count is
        bs <- getbranches is
        bsm <- getmother
        seed <- getstp st_seed
        md <- getstp st_md

        let -- md = bs_dev bsm
            pins = map bs_pin bs
            dfs = map (\pin -> df_shape $ get_disfunc seed pin) pins
            arl = map (\bs -> Children (if (md == 0) then 0.0 else  (bs_pos bs)/md)) bs
            dvl = map bs_dev bs
            psl = map bs_pos bs
            mdl = map (\bs -> md - (bs_pos bs)) bs
            dl = mapf5 dfs arl bcl psl dvl mdl
            
--        when (not $ null dl) (mprint $ (show $ zip arl dl))

        return dl
                          
bdl::SimState s ([Float], Float)
bdl = do error "bdl has been depricated by bdli."
         st <- getst
         bsm <- getmother
         bsmi <- getmotheri
         bs <- getbranches (bs_bs bsm)
         lcl <- mapM branch_count (bs_bs bsm)
         let (dfnc, strict) = let df = get_disfunc (st_seed st) ((bs_pin bsm) ++ [0]) 
                              in (df_shape df, df_strict df)
             arl = map (\bs-> Children ((bs_pos bs)/(bs_dev bsm))) bs -- if age == 0 then (pos/dev) else (pos/dev)) bs
             del = map (\bs -> (bs_dev bs)) bs
             psl = map bs_pos bs
             mdl = map (\bs -> (bs_dev bsm)- (bs_pos bs)) bs
             reti = map5 dfnc arl lcl psl del mdl
             ret = map (\x -> x) reti
          
         return (ret, strict)

stillhungry::Int -> Int -> SimState s Bool
stillhungry n i = do
       if (n == 0) then return False
                   else do
                       let inp = 0.001
                       bs <- getbranch i
                       outp <- moddev i inp

                       rbsi <- filterM isBranchBsi (bs_bs bs)
                       rec <- mapM (stillhungry (n - 1)) rbsi
                       let r = foldr (||) (inp == outp) rec
                       
                       return $ r
                        
moddev::Int -> Development -> SimState s Development
moddev i d = do
        bs <- getbranch i
        st <- getst
        seed <- getstp st_seed

        let mdf = get_moddevfnc (bs_pin bs) seed     
        if (null $ bs_min bs) then return d
                              else return $ mdf bs (st_md st) d


getdist::Int -> SimState s (Float, Float)
getdist i = do 
             st <- getst
             bs <- getbranch i            
             bc <- branch_count i
             let param = if (bs_min bs) == [] then RootSelf else Self -- if ma == 0 then 1.0 else 1.0 -- (pos / ma)
                 mdd = (st_md st) - (bs_pos bs)
                 df = (get_disfunc (st_seed st) (bs_pin bs)) 
                 d = (df_shape df) param bc (bs_pos bs) (bs_dev bs) mdd

             return (d, df_strict df)

        

my_dist::SimState s (Float, Float)
my_dist = do st <- getst
             bs <- getmother
             bsi <- getmotheri
             bc <- branch_count bsi
             let param = if (bs_min bs) == [] then RootSelf else Self -- if ma == 0 then 1.0 else 1.0 -- (pos / ma)
                 mdd = (st_md st) - (bs_pos bs)
                 df = (get_disfunc (st_seed st) (bs_pin bs)) 
                 d = (df_shape df) param bc (bs_pos bs) (bs_dev bs) mdd
            --  mprint $ (show (bs_pin bs)) ++ " " ++ (show d) ++ " where (dev , mdd): " ++ (show $ (bs_dev bs, mdd)) ++
            --          " (md, pos): " ++ (show (st_md st, bs_pos bs))
            -- adderr $ show (bs_pin bs, bc, ret)
--             when (bs_pin bs == [0,0]) (mprint $ (show (d, param, bc, bs_dev bs, mdd)) ++ " <- mydist")
             return (d, df_strict df)

getnewq::SimState s Quat  
getnewq = do bsm <- getmother
             st <- getst
             let q = snd $ st_pos st
                 ret = mulq q (rot_q_y (bs_dir bsm))                                                                                 
             return ret

devToPow::Development -> SimState s Power
devToPow d = do
        t <- gettime               
        grow <- getgrow
        return $ if t == 0 then 0 else d / (t * grow)

powToDev::Power -> SimState s Development
powToDev p = do
        t <- gettime
        grow <- getgrow
        return $ p * t * grow

pos_and_q_i::[Int] -> SimState s [(Point, Quat)]
pos_and_q_i is = do
        q <- getnewq
        bsmi <- getmotheri
        ps <- mapM (getMSPos bsmi) is
--        tbs <- getbranches is
--        let ids = map bs_pin tbs
--        when (bsmi == 0) (mprint $ (show (ps, ids)) ++ " <- pos")
        return $ zip ps $ repeat q

pos_and_q::SimState s [(Point, Quat)]
pos_and_q = do
        bsm <- getmother
        pos_and_q_i (bs_bs bsm)
{-
pos_and_q = do st <- getst                    
               q <- getnewq
               bsm <- getmother
               bsmi <- getmotheri
               let mspos = fst $ st_pos st
               ps <- mapM (getMSPos bsmi) (bs_bs bsm)
               let ret = zip ps (repeat q)
               return $ if (null $ st_cbs st) then repeat (origo, id_quat)
                                              else ret
-}
branch_count::Int -> SimState s Int
branch_count i = do
        bs <- getbranch i
        bss <- mapM getbranch (bs_bs bs)
        rc <- mapM branch_count (bs_bs bs)
        return $ (length $ filter (\x -> f (bs_mode x)) bss) + (sum rc)

        where f Leaf = False
              f Branch = True

getpos::SimState s ModelPos
getpos = do st <- getst
            return (st_pos st)
            
setpos::ModelPos -> SimState s ()
setpos pos = modst (\st -> st {st_pos = pos})

collectpower::SimState s Power
collectpower = do
        (Left pb) <- poppowback
        modst (\st -> st {st_powacc = 0.0})
        return $ pb

getpowback::SimState s Power
getpowback = do
        epb <- getstp st_powback
        let (Left pb) = epb
        return pb
         
poppowback::SimState s (Either Power Power)
poppowback = do st <- getst
                modst (\st -> st {st_powback = Left 0.0})
                return $ st_powback st
                
pushpowback::(Either Power Power) -> SimState s ()
pushpowback (Left x) = do st <- getst
                          modst (\st -> st {st_powback = Left x})


addpowback::Float -> SimState s ()
addpowback p = do st <- getst
                  let (Left x) = st_powback st
                  modst (\st -> st {st_powback = Left (x + p)})

updaterad::SimState s ()
updaterad = do (bsmi, bsm) <- getmothert
               branchnode <- get_plantnode (bs_pin bsm)
               st <- getst
               new_rad <- mass_count bsmi
               modmother (\bs -> bs {bs_rad = (sqrt $ sqrt new_rad) * (br_thick branchnode)})

updatepnt::SimState s ()
updatepnt = do
        pos <- SimStateHelpers.getpos
        bsmi <- getmotheri
        bsm <- getmother
        seed <- getstp st_seed
        let p0 = getlocpos seed bsm (bs_dev bsm)
            qf = (snd pos) `mulq` (rot_q_y (bs_dir bsm))
            p1 = (fst pos) `add` rot_vec_q p0 qf
 
--        when ((bs_pin bsm) == [0,0])(mprint $ (show $ (fst pos)) ++ " / " ++ (show $ bs_pin bsm) ++ " <- posi")
        modbranch bsmi (\bs -> bs {bs_pnt = (fst pos)})
            			                                           
clearerr::SimState s ()
clearerr = do st <- getst 
              setst $ (st {st_err = ""})

adderr::String -> SimState s ()
adderr s = do st <- getst
              setst $ st {st_err = (st_err st) ++ "\n" ++ s}

addnewchild::BranchState -> SimState s ()
addnewchild bs = do
        bsi <- barrNewBS bs
        addchild bsi
        
addchild::Int -> SimState s ()
addchild ci = addchildren [ci] 

addchildren::[Int] -> SimState s ()
addchildren bssi = do 
        bsm <- getmother
        let nbssi = (bs_bs bsm) ++ bssi
        modmother (\m -> m {bs_bs = nbssi})
                     
popchildren::SimState s [Int]
popchildren = do bs <- getmother
                 modmother (\m -> m {bs_bs = []})
                 return $ bs_bs bs                    
{-                    
popmother::SimState s BranchState
popmother = do st <- getst
               bsm <- getmother
               let nbsm = bs_bs bsm
               -- adderr $ show nbsm
               modst (\st -> st {st_bs = nbsm})
               return bsm

pushmother::BranchState -> SimState s ()
pushmother bs = do st <- getst
                   let bss = st_bs st
                       bsn = bs {bs_bs = bss}
                   modst (\st -> st {st_bs = [bsn]})
                   return ()               
-}

addpowout::Power -> SimState s ()
addpowout np = do
        cp <- getstp st_powout
        modst (\st -> st { st_powout = cp + np })

pushaccpow::SimState s ()
pushaccpow = do st <- getst
                let newpow = st_powout st + st_powacc st
                modst (\st -> st {st_powout = newpow, st_powacc = 0.0})

getpowout::SimState s Power
getpowout = do st <- getst
               return (st_powout st)

setpowout::Power -> SimState s ()
setpowout pow = do modst (\st -> st {st_powout = pow}) 
                   return ()
        
getpowacc::SimState s Power
getpowacc = do st <- getst
               return (st_powacc st)
               
backtoacc::SimState s ()
backtoacc = do st <- getst
               let (Left p) = st_powback st
                   pa = st_powacc st
                   newacc = pa + p
               modst (\st -> st {st_powacc = newacc, st_powback = Left 0.0})

{-
newmother::BranchState -> SimState s ()
newmother bsm = do modst (\st -> st {st_bs = [bsm]})

getmother::SimState s BranchState
getmother = do st <- getst
               let bs = head $ st_bs st
               return bs

modmother::(BranchState -> BranchState) -> SimState s ()
modmother f = do bsm <- getmother
                 newmother (f bsm)
-}
gettime::SimState s Time
gettime = do st <- getst
             return $ st_time st
             
settime::Time -> SimState s ()
settime t = do modst (\st -> st {st_time = t})
               return ()

getrndgen::SimState s Rnd
getrndgen = do
        rnd <- getstp st_rnd
        let (r0, r1) = split rnd
        modst (\st -> st {st_rnd = r0})
        return r1

getran::SimState s Float
getran = do st <- getst
            let r = st_rnd st
                (rnd, r') = randomR (0,1) r
            modst (\st -> st {st_rnd = r'})
            return rnd
            
getfwd = do st <- getst
            bsm <- getmother
            let qtmp = rot_q_y (bs_dir bsm) 
                q = snd $ st_pos st
                nq = mulq q qtmp
                fwd = rot_vec_q (0,1,0) nq
            return (bs_dir bsm)

getnewbsid::SimState s Int                                    
getnewbsid = do st <- getst
                let bsid = (st_idcnt st)
                modst (\s -> s {st_idcnt = bsid + 1})
                return bsid

getnewbranchid = do st <- getst
                    bs <- getmother
                    let branchid = (take ((length (bs_min bs)) + 1) (bs_pin bs))
                    return branchid
                  
getndir pin fwd = do st <- getst
                     rnd <- getran
                     let c = findcrown pin (st_seed st)
                     let ret = if pin == [] then (0,1,0) else (dirfunc rnd fwd c)
                     return ret


getsprouts = do st <- getst
                lpin <- getlpin
                let inf = findif lpin (st_seed st)
                    sprouts = (if_scnt inf)
                return sprouts
                  
getleaf = do st <- getst
             bsm <- getmother
             lpin <- getlpin
             let leaf = findcrown lpin (st_seed st)
             return leaf
             
getlpin = do st <- getst
             bsm <- getmother
             let lpin = findleaf (bs_pin bsm) (st_seed st)
             return lpin

kill_leaves::Int -> SimState s ()
kill_leaves i = 
              do st <- getst
                 bsm <- getbranch i
                 case findnode (bs_pin bsm) (st_seed st) of
                   Nothing -> return ()
                   Just (Le _ _ _ _ _ _) -> modbranch i (\bs -> bs {bs_alive = Dead})
                   Just (Br _ _ _ _ _) -> mapM_ kill_leaves (bs_bs bsm) -- newmother (bsm {bs_bs = map (f (st_seed st)) (bs_bs bsm) }
{-   where
         f s bs = case findnode (bs_pin bs) s of
                   Nothing -> bs
                   Just (Le _ _ _ _ _) -> bs {bs_alive = False}
                   Just (Br _ _)     -> bs {bs_bs = (map (f s) (bs_bs bs))}
-}
---------------------------------------

get_efnc::PlantId -> SimState s EvFnc
get_efnc pin = do st <- getst
                  let inf = findif pin (st_seed st)
                      efnc = if_evf inf
                  return efnc

get_plantnode::PlantId -> SimState s PlantNode
get_plantnode pin = do
        seed <- getstp st_seed
        let (Just pn) = findnode pin seed
        return pn

