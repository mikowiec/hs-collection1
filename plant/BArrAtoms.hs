module BArrAtoms where


import qualified STArrLib as Arr
import NonBasic
import PlantTypes
import MonadLib
import Monad
import IOExts 

lmprint d = getst >>= (\gst -> (setst (trace d gst)))

barrInsBS:: Int -> BranchState -> SimState s ()
barrInsBS i val = do
        Arr.ins i val
        return ()

barrNewBS:: BranchState -> SimState s Int
barrNewBS bs = do
        max <- getstp (barr_max.st_barr)
        curr <- getstp (barr_curr.st_barr)
        barrInsBS curr bs        
        inccurr
        return curr

barrSetBS:: Int -> BranchState -> SimState s ()
barrSetBS i val = do
        curr <- getstp (barr_curr.st_barr)
        when (i > curr) (error "BArrAtoms::barr_setbs not yet allocated")
        Arr.ins i val
        return ()

inccurr:: SimState s ()
inccurr = do
        curr <- getstp (barr_curr.st_barr)
        barr <- getstp st_barr
        let nbarr = barr {barr_curr = curr + 1}
        modst (\st -> st {st_barr = nbarr})

barrGetBS:: Int -> SimState s BranchState
barrGetBS i = do
       curr <- getstp (barr_curr.st_barr)
       when (i > curr) (error "BArrAtoms::barr_barrGetBS - not yet allocated")
       ret <- Arr.gte i
       return ret
       
------------------------------------------------------
-- mother operations
------------------------------------------------------

newmother::Int -> SimState s ()
newmother i = do modst (\st -> st {st_cbs = [i]})

getmother::SimState s BranchState
getmother = do
        bsi <- getstp (head.st_cbs)
        bs <- barrGetBS bsi
        return bs

setmotheri::Int -> SimState s ()
setmotheri i = do
        modst (\st -> st {st_cbs = [i]})

getmotheri::SimState s Int
getmotheri = do
        bsi <- getstp (head.st_cbs)
        return bsi

getmothert::SimState s (Int, BranchState)
getmothert = do
        bsi <- getstp (head.st_cbs)
        bs <- barrGetBS bsi
        return (bsi, bs)

modbranch::Int -> (BranchState -> BranchState) -> SimState s ()
modbranch i f = do
        bs <- getbranch i
        barrSetBS i (f bs)       

modbranchr::Int -> (BranchState -> BranchState) -> SimState s ()
modbranchr i f = do
        modbranch i f
        bsm <- getbranch i
        mapM_ (`modbranchr` f) (bs_bs bsm)        

modbranches::[Int] -> (BranchState -> BranchState) -> SimState s ()
modbranches is f = mapM_ (`modbranch` f) is

modmother::(BranchState -> BranchState) -> SimState s ()
modmother f = do 
        bsi <- getmotheri
        bs <- getmother
        barrSetBS bsi (f bs)

popmother::SimState s Int
popmother = do st <- getst
               bsmi <- getmotheri
               bsm <- getmother
               let nbsm = bs_bs bsm
               modst (\st -> st {st_cbs = nbsm})
               return bsmi

getbranch::Int -> SimState s BranchState
getbranch i = barrGetBS i

getbranchp::Int -> (BranchState -> a) -> SimState s a
getbranchp i f = do
        bs <- getbranch i
        return $ f bs
        
getbranches::[Int] -> SimState s [BranchState]
getbranches is = mapM getbranch is

pushmother::Int -> SimState s ()
pushmother bsi = do 
        st <- getst
        let bssi = st_cbs st
        nbsm <- barrGetBS bsi
        barrSetBS bsi (nbsm {bs_bs = bssi})
        modst (\st -> st {st_cbs = [bsi]})
        return ()               

addbss::State s -> SimState s ()
addbss ost = do st <- getst
                let bss = st_cbs ost
                    bs = head $ st_cbs st
                modst (\st -> st {st_cbs = (bs:bss)})
                return ()

