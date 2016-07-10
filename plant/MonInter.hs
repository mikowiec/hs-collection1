module MonInter where

import NonBasic
import PlantTypes
import MonadLib
import Monad
import Types
import Helpers hiding (getpos)
import Misc
import LinAlg
import List
import Random
import SimStateHelpers         
import ST
import STArrLib
import BArrAtoms
import Wither

mclone::TreeState s ()
mclone = do
    tree <- getst
    st <- getstp tree_state
    -- Thaw State
    tst <- thawState st
    s <- lift $ (do (s,_) <- runstp (clonesetup) tst
                    return s)
   
    -- Freeze State
    fst <- freezeState s
    modst (\tree -> tree {tree_state = fst})
    return ()

clonesetup::SimState s (State s)
clonesetup = do
    bsmi <- getmotheri
    clone bsmi
    st <- getst
    return st
              
msimula::TreeState s ()
msimula = do 
   tree <- getst                        
   st <- getstp tree_state
   (pt:pts) <- getstp tree_simlist
  
   -- Thaw State
   tst <- thawState st
   s <- lift $ (do (s,_) <- runstp (simsetup pt) tst
                   return s)
  
   -- Freeze State
   fst <- freezeState s
   modst (\tree -> tree {tree_state = fst, tree_simlist = pts, tree_rep = Nothing})
   return ()

                                   
simsetup::(Resrc, Time) -> SimState s (State s)
simsetup (rsc,t) = do 

    mprint $ (show rsc) ++ ", " ++ (show t) ++ " <- power, time"
    -- Genesis if necesarry
    cbs <- getstp st_cbs
    when (null cbs) genesis
    
    -- Prepare Simulation
    st <- getst
    (bsmi, bsm) <- getmothert
    mass <- mass_count bsmi
    earth <- getstp (se_earth.st_seed)
    let root_p = (ea_rootf earth) mass rsc
        initpos = (origo, id_quat)
        nst = st {st_time = t, st_powout = root_p, st_powback = Left 0.0, st_pos = initpos}
    setst nst                      
    writeflag FeedLeaves True
    pushaccpow

    printstats

    lcnt <- collectstats bsmi
    mprint $ (show lcnt) ++ " <- lcnt mother "
    
    -- Simulate
    
    feedleaves2
    msim                   
    backtoacc
    
    getst
                   
msim::SimState s (State s)
msim = do 
        feedbranchesim       
  
        cp <- collectpower
        addpowout cp

        withsun       
        terminatewithered

        sim 
        updaterad
        updatepnt
 
        getst
    where                               
       
         simsun::SimState s ()
         simsun = do
                (bsmi, bsm) <- getmothert
                brids <- filterM isBranchBsi (bs_bs bsm)
                leids <- filterM isLeafBsi (bs_bs bsm)

                als0 <- mapBsI (\bs -> ((bs_dev bs) / (bs_dev bsm)) > 0.15) brids
                als1 <- mapBsI (\bs -> (bs_dev bs) < 0.05) brids
                als2 <- mapBsI (\bs -> ((length $ bs_pin bs) - (length $ bs_pin bsm)) < 2) brids
                let bos = map3 (\a b c -> (a || b) ||c) als0 als1 als2

                let (living_br, dead_br) = (\(a,b) -> (map fst a, map fst b)) $ partition snd  (zip brids bos)
               
                let --  living_br = (map fst $ filter snd $ zip brids bos) 
                    (living_le, dead_le) = if (bs_rad bsm) < 0.3 then (leids, []) else ([], leids)

                dd <- mapM leaf_count dead_br
                letthemfall (dead_br ++ dead_le)

                modbranch bsmi (\bs -> bs {bs_bs = (living_br ++ living_le)})

                return ()


         feedleavesim::SimState s ()
         feedleavesim = do
                p <- getpos
                flb <- readflag FeedLeaves          
                when flb (do
                        bsm <- getmother
                        lids <- filterM isLeafBsi (bs_bs bsm)
                        pqs <- pos_and_q_i lids
                        feedleaves2i lids pqs
                        setpos p)
                   

         feedleaves2i::[Int] -> [ModelPos] -> SimState s ()
         feedleaves2i [] _ = return ()
         feedleaves2i (i:is) (p:ps) = do
                pow <- getpowout
                lf <- getleaffood i
                if (lf > pow) then do killmei i

                              else do lifemei i
                                      setpowout $ pow - lf
        
                feedleaves2i is ps

         feedlev::[Int] -> SimState s ()
         feedlev is = do
                bsmi <- getmotheri
                bss <- getbranches is
                power <- getpowout
                
                cml <- mapM mass_count is
                lim <- mapBsI bs_last is

                let mass = sum cml
                
                undl <- bdli is
                let ndl = normlist undl
                    dml = (map (mass *) ndl) 
                    massdiff = negtozerolist $ map2 (-) dml cml

                powdiff <- mapM devToPow massdiff
                let eql = limitlist power powdiff                                    
                pqs <- pos_and_q_i is

                feedbranches2i is eql pqs                

                addpower <- collectpower
                st <- getst
                let remp = power - (sum eql) + addpower
                setpowout remp
                setmotheri bsmi

         feedbranchesim::SimState s ()
         feedbranchesim = do                
                
                (bsmi, bsm) <- getmothert
                (mydist, _) <- my_dist 
                power <- getstp st_powout
                pushvitals 
                modst (\st -> st {st_md = bs_dev bsm})

                -- Find branches
                brids_p0 <- filterM isBranchBsi (bs_bs bsm)
                brids <- filterM (stillhungry 1) brids_p0
                pbrids <- partBsI brids

                -- Feed to equalibrium
                mapM feedlev pbrids 

                -- Feed the hungry
                bridsmore <- filterM (stillhungry 1) brids

                undl <- bdli bridsmore
                pqs <- pos_and_q_i bridsmore
                remp <- getpowout
                let (myp:rpd) = map (remp *) (normlist (mydist:undl))
                feedbranches2i bridsmore rpd pqs                

                -- Prepare for exit
                modst (\st -> st {st_powout = myp})
                popvitals

         feedbranches2i::[Int] -> [Power] -> [ModelPos] -> SimState s ()
         feedbranches2i [] _ _ = return ()
         feedbranches2i (i:is) (p:ps) (m:ms) = do
               omd <- getstp st_md
               opos <- getstp st_pos
               feedbranches2i is ps ms
               bsm <- getmother
               modst (\st -> st {st_cbs = [i], st_powout = p, st_pos = m})
               pb <- collectpower
               msim              
               addpowback pb
               modst (\st -> st {st_md = omd, st_pos = opos})
        
         sim::SimState s ()
         sim = do st <- getst
                  g <- getgrow
                  bsm <- getmother
                  
                  bsmi <- getmotheri
                  
                  if (isLeafBs bsm) then simleaf else (do 
                            inpow <- getpowout
                            stdtime <- gettime
                            st <- getst
                            bsm <- getmother
                            grow <- getgrow
                            time <- gettime
                            -- Find next event
                            (pin, dev) <- get_nxtevdev
                            let events = not $ null pin
                                dtime = (dev - (bs_dev bsm)) / (inpow * grow)
                                t1 = dtime
                                t2 = time - t1  
                            
                            -- if event occurs within given timeframe then
                            if events && inpow > 0 && dtime < time then do 
                  
                                settime t1
                                dev_ok <- sim2im
                                if dev_ok then do 
                                    settime t2
                                    -- execute event
                                    addlev pin
                                    ofl <- readflag FeedLeaves
                                    writeflag FeedLeaves False
                                    msim >> return ()
                                    writeflag FeedLeaves ofl
                                          else do 
                                    return ()
                                                                      
                            -- else simply grow                                                                      
                                                                   else do 
                  
                                sim2im
                                return ()

                            settime stdtime
                            return ())
                            
         simleaf::SimState s ()
         simleaf = do st <- getst
                      bsm <- getmother
                      when (not $ isLeafBs bsm) (error "simleaf cannot feed branches!")
                      let (Just leaf) = findnode (bs_pin bsm) (st_seed st)
                          inpow = st_powout st
                          food = le_cons leaf
                          starve = food < inpow
                      if starve then do killme
                                else do lifeme
                                        modst (\st -> st {st_powout = inpow - food})
                      return ()

         killmei::Int -> SimState s ()
         killmei i = do
                modbranch i (\bs -> bs {bs_alive = Dead})

         lifemei::Int -> SimState s ()
         lifemei i = do
                modbranch i (\bs -> bs {bs_alive = birth_phys})

         killme::SimState s ()
         killme = do 
                bsi <- getmotheri
                modbranchr bsi (\bs -> bs {bs_alive = Dead})

         lifeme::SimState s ()
         lifeme = do 
                bsi <- getmotheri
                modbranchr bsi (\bs -> bs {bs_alive = birth_phys})
       
         getdev::SimState s Development
         getdev = do
                power <- getpowout
                delta_dev <- powToDev power
                return delta_dev

         sim2im::SimState s Bool
         sim2im = do
                omd <- getstp st_md
                (bsmi, bsm) <- getmothert
                dev <- getdev 
                time <- gettime

--                mprint $ (show $ (bs_pin bsm, bs_min bsm, omd))
                mdev <- moddev bsmi dev
                overflow <- devToPow (dev - mdev)
                addpowback overflow
                let more = overflow < 0.0001
                modbranch bsmi (\bs -> bs { bs_dev = (bs_dev bsm) + mdev, bs_more = more, bs_age = (bs_age bsm) + time})
                return $ more
                
newleaf::PlantId -> SimState s () 
newleaf lpin = do 
             st <- getst
             bsm <- getmother
             rng <- getrndgen
             fwd <- getfwd
             leaf <- getleaf
             dir <- getndir lpin fwd
             pos <- getpos
             let (_,y,_) = dir
             newbsid <- getnewbsid
             let nbs = (BS [] dir (fst pos) (bs_dev bsm) 0.0 0.0 0.0 Leaf lpin (bs_pin bsm) birth_phys True newbsid [] init_last rng 1)
             addnewchild nbs
             tbsm <- getmother
             nxtleaf <- get_nxtev lpin (bs_dev bsm)
             modmother (\bs -> update_nxtev lpin (snd nxtleaf) bs)

newbranchim pin = do
        cnt <- get_interfacep pin if_scnt
        newbranch pin (max cnt 1)

        

newbranch pin 0 = return ()
newbranch pin n = do 
   bsm <- getmother
   let min = bs_pin bsm
   st <- getst
   if isLeaf pin (st_seed st) then do   
        newleaf pin
                              else do
        rnd <- getran
        rng <- getrndgen
        efm <- get_efnc pin
        let nv = efm rnd (bs_dev bsm)                                                                                                  
        modmother (\bs -> update_nxtev pin nv bs)
        fwd <- getfwd
        ndir <- getndir pin fwd
        newbsid <- getnewbsid
        let lpin = findleaf pin (st_seed st)
            npin = pin ++ [0]
        evl <- get_nxtevs npin lpin
        pos <- getpos
        let nbs = BS [] ndir (fst pos) (bs_dev bsm) 0.0 0.0 0.0 Branch pin min (Alive 1.0) True newbsid evl init_last rng 0
        addnewchild nbs
   newbranch pin (n - 1)

addlev::PlantId -> SimState s ()
addlev pin = do let cpin = pin
                newbranchim cpin

get_nxtevdev::SimState s (PlantId, Development)
get_nxtevdev = do st <- getst
                  bsm <- getmother
                  let nxtl = bs_nxtev bsm
                      ret = f nxtl
                  if null nxtl then return ([], 0.0)
                               else return ret
            
            where     f (a:[])  = a
                      f (a@(i,v):ivs) = let b@(i',v') = f ivs
                                        in if v < v' then a else b

get_nxtevs:: PlantId -> PlantId -> SimState s [(PlantId, Development)]
get_nxtevs bpin epin = do let pins = filter (\p -> (length p) >= (length bpin)) $ inits epin
                              md = 0.0
                          evs <- mapM (`get_nxtev` md) pins
                          return evs
                    
                    
get_nxtev::PlantId -> Development -> SimState s (PlantId, Development)
get_nxtev pin md = do rnd <- getran
                      efm <- get_efnc pin
                      let nv = efm rnd md
                      return (pin, nv)
         
update_nxtev::PlantId -> Development -> BranchState -> BranchState
update_nxtev pin nv bs = let onxtev = bs_nxtev bs
                             nnxtev = map (f pin nv) onxtev
                             nbs = bs {bs_nxtev = nnxtev}
                         in nbs
    where f ri rv (i,v) = if (i == ri) then (i,rv) else (i,v)
    
genesis:: SimState s ()
genesis = do 
        let pin = [0]
        rnd <- getran
        efm <- get_efnc pin
        let nv = efm rnd 0.0
        let fwd = (0,1,0)
        ndir <- getndir pin fwd
        newbsid <- getnewbsid
        seed <- getstp st_seed
        let lpin = findleaf pin seed
            npin = pin ++ [0]
        evl <- get_nxtevs npin lpin
        rng <- getrndgen
        let nbs = BS [] ndir origo 0.0 0.0 0.0 0.0 Branch pin [] (Alive 1.0) True newbsid evl init_last rng 0
        bsi <- barrNewBS nbs
        newmother bsi
