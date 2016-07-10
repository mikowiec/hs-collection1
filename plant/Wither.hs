module Wither where

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

 
terminatewithered::SimState s () 
terminatewithered = do
        bsmi <- getmotheri
        cutbranches bsmi

cutbranches::Int -> SimState s ()
cutbranches pin = do
        bs <- getbranch pin
        fall <- branchfalloff pin
        brids <- filterM isBranchBsi (bs_bs bs)
        if fall then letthemfall [pin]
                else mapM_ cutbranches brids

withsun::SimState s ()
withsun = do
        (bsmi, bsm) <- getmothert
        brids <- filterM isBranchBsi (bs_bs bsm)
        return ()
        mapM_ (Wither.tan brids) brids

tan::[Int] -> Int -> SimState s ()
tan ns pin = do
        let cs = delete pin ns
        bl <-  mapM (in_shade pin) cs
        let shaded = or $ bl                
        --                when shaded (letthemfall [pin])
        when shaded (letthemwither pin)

in_shade::Int -> Int -> SimState s Bool 
in_shade pin cpin = do 
        bs <- getbranch pin
        cbs <- getbranch cpin
        let dis cbs = (bs_pos cbs) - (bs_pos bs)
            beneath cbs = (dis cbs) > 0.0
            dif cbs = (bs_dev cbs) - (bs_dev bs)
            fal cbs = (dif cbs) / (dis cbs) > 1.0
            rem cbs = beneath cbs && fal cbs 
        return $ rem cbs

letthemwither::Int -> SimState s ()
letthemwither pin = do
        bs <- getbranch pin
        brids <- filterM isBranchBsi (bs_bs bs)
        leids <- filterM isLeafBsi (bs_bs bs)  
        mapM_ (wither 1.0) leids
        mapM_ letthemwither brids

wither::Float -> Int -> SimState s ()
wither speed pin = do
        bs <- getbranch pin                 
        time <- gettime
        let ps = bs_alive bs
        case ps of
            Dead -> return ()
            Alive _ -> do let nmood = (ps_mood ps) - (speed * time)
                              nps = if (nmood > 0) then (bs_alive bs) {ps_mood = nmood} 
                                                   else Dead
                          modbranch pin (\bs -> bs {bs_alive = nps})


unwither::Float -> Int -> SimState s ()
unwither speed i = do
        bs <- getbranch i
        time <- gettime
        
        let ps = bs_alive bs
            alife = (time * speed)
        nval <- case ps of
                    Dead -> return $ max alife 1.0
                    Alive _ -> return $ max 1.0 (alife + (ps_mood ps))
        modbranch i (\bs -> bs {bs_alive = (Alive nval)})

branchfalloff::Int -> SimState s Bool 
branchfalloff pin = do
        bs <- getbranch pin
        leids <- filterM isLeafBsi (bs_bs bs)
        brids <- filterM isBranchBsi (bs_bs bs)
        vals <- mapM lifepower leids
        let w = ((sum vals) + (intToFloat $ length brids)) / (intToFloat $ length leids)
            threshold = 0.5
        return $ w < 0.5

lifepower::Int -> SimState s Float
lifepower pin = do
        bs <- getbranch pin
        case (bs_alive bs) of
            Dead -> return 0.0
            Alive x -> return x

feedleaves2::SimState s ()
feedleaves2 = do
        bsmi <- getmotheri
        pow <- getpowout
        powleft <- leaves_f pow bsmi
        mprint $ (show powleft) ++ " of " ++ (show pow) ++ " left."
        setpowout powleft

leaves_f:: Float -> Int -> SimState s Float
leaves_f pow i = do        
        bs <- getbranch i
        brids <- filterM isBranchBsi (bs_bs bs)
        leids <- filterM isLeafBsi (bs_bs bs)
        rest <- foldM leaves_g pow leids 
        ret <- foldM leaves_f rest brids
        return ret

leaves_g:: Float -> Int -> SimState s Float
leaves_g pow i = do
        cons <- getleaffood i
        if cons < pow then do unwither 10.0 i
                              return $ pow - cons
                      else do wither 10.0 i
                              return pow


