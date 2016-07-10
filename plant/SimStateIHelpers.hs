module SimStateIHelpers where

import NonBasic
import Data.Array
import PlantTypes
import MonadLib
import LinAlg
import Types
import Helpers
import IOExts

getmother::SimStateI BranchState
getmother = do
        bsi <- getstp (head.ist_cbs)
        getbranch bsi

mass_count::Int -> SimStateI Float
mass_count i = do
        return 1

getbranches::[Int] -> SimStateI [BranchState]
getbranches is = do
        mapM getbranch is 

getbranchp::Int -> (BranchState -> a) -> SimStateI a       
getbranchp i f = do
        bs <- getbranch i
        return $ f bs

getbranch::Int -> SimStateI BranchState
getbranch i = do
        arr <- getstp (ibarr_arr.ist_barr)
        return $ arr!i 

mprint d = getst >>= (\gst -> (setst (trace (reverse $ reverse $ d) gst)))

getleaf::Int -> SimStateI PlantNode        
getleaf i = do
        seed <- getstp ist_seed
        bs <- getbranch i
        let (Just leaf) = findnode (bs_pin bs) seed
        return leaf


grav2::Int -> ModelPos -> Development -> SimStateI Point
grav2 i (p,q) dev = do
        bs <- getbranch i
        seed <- getstp ist_seed
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

grav3::Int -> ModelPos -> Development -> SimStateI Point
grav3 i (p,q) dev = do
        bs <- getbranch i
        seed <- getstp ist_seed
        let p0 = getlocpos seed bs dev
            p1 = rot_vec_q p0 q
            p2 = p1 `add` p
        return $ p `add` (mulk dev (bs_dir bs))
