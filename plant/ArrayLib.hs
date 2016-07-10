module ArrayLib where

import Array
              
               
arr_insert::(Num a, Ix a, Enum a) => Array a b -> (a,b) -> Array a b
arr_insert  arr (i,v) = let imax = snd $ bounds arr
                            carr = if i > imax then (double_until_arr arr i) else arr
                        in carr//[(i,v)]

double_until_arr::(Num a, Ix a, Enum a) => Array a b -> a -> Array a b
double_until_arr arr lim = let (fi, li) = bounds arr
                               narr = incarr arr (li - fi)
                               nli = snd $ bounds narr
                           in if nli < lim then (double_until_arr narr lim) else narr

incarr::(Num a, Ix a, Enum a) => Array a b -> a -> Array a b
incarr arr size = let (fi, li) = bounds arr
                      nli = li + 1 + size
                      oal = (assocs arr)
                      nal = oal ++ [(i,snd $ head oal) | i <- [li + 1..nli]] 
                      narr = array (fi, nli) nal 
                  in narr

-- export Dynamic Array

data DArray a b = DArr {
                    da_arr :: Array a b,
                    da_nxt :: a
                  } deriving Show

init_darray::(Num a, Ix a, Enum a) => a -> b -> DArray a b
init_darray i v = let arr = array (i,i) [(i,v)]
                  in DArr arr i

(<|)::(Num a, Ix a, Enum a) => DArray a b -> b -> (a, DArray a b)
(<|) arr val = let narr = (da_arr arr) `arr_insert` ( da_nxt arr, val)
                   nmax = succ $ da_nxt arr
               in (da_nxt arr, DArr narr nmax)

(|>)::(Num a, Ix a, Enum a) => DArray a b -> a -> b
(|>) arr i = if i >= (da_nxt arr) then error "DArray::Can't read unclaimed space, mate!"
                                  else (da_arr arr)!i

                                  

mk_plants :: IO [(String, SimEngine)]
mk_plants = sequence (
    [])


