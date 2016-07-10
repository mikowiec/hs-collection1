module STArrLib where



import MonadLib
import Monad
import ST
import Data.Array.MArray
import Data.Array.ST
import PlantTypes
import NonBasic

lift m = ST (\s -> do v <- m; return (v,s))

{-
prog :: StateMonadP (BArr s) (ST s) BranchState
prog = do
    st <- getstp id
    lift $ test st    
    a <- getstp st_arr
    s <- lift (getElems a)
    return (head s)
-}


newBArr :: ST s (BArr s)
newBArr = do 
          a <- newListArray (0,0) (repeat null_state)
          return (BArr a 0 0)
             
ins::Int -> BranchState -> SimState s () 
ins i v = do max <- getstp (barr_max.st_barr)
             barr <- getstp st_barr
             arr <- getstp (barr_arr.st_barr)
             when (i > max) ( do let size = nsize max i
                                 narr <- lift $ resize arr size
                                 let nbarr = barr {barr_arr = narr, barr_max = size}
--                                 error $ "!!!" ++ (show size) ++ " " ++ (show max) ++ " " ++ (show i)
                                 modst (\st -> st {st_barr = nbarr}) )
             warr <- getstp (barr_arr.st_barr)
             lift $ writeArray warr i v
             return ()
             
             where nsize c n = if c < n then nsize (2 * c + 1) n
                                        else c
             


gte::Int -> SimState s BranchState
gte i = do arr <- getstp (barr_arr.st_barr)
           val <- lift $ readArray arr i
           return val

resize::STArray s Int BranchState -> Int -> ST s (STArray s Int BranchState)
resize arr size = do let (f,l) = bounds arr
                     es <- getElems arr
                     let nes = es ++ nes
                     let nl = f + size
                     narr <- newListArray (f, nl) nes
                     return narr
{-
test::BArr s -> ST s ()
test s = do let arr = st_arr s
            writeArray arr 1 (null_state {bs_dev = 666})
            return ()

mk_state :: ST s (BArr s)
mk_state = do
          a <- newListArray (0,0) (repeat null_state)
          return (BArr a 0 0)

main = do 
     print (runST (do s <- mk_state
                      (r,_) <- runstp prog s
                      return r))
-}
