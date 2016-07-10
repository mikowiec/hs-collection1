
module Main where

import System
import Char

import LinAlg

import Pf

import Misc

import Types
import PlantTypes
import Povray
import Random

import qualified GeomRep as G

import SimEngine
import qualified LSystem as LS

{-
run = do
    r <- newStdGen
    t@(Tree s ps e bs rep) <- init_tree
    let f (r,(p:ps,bs)) =
         let (r',r'') = split r
             bs'      = simula r' s e p bs
         in (r'', (ps,bs'))
    return $ map (\(_,(_,b)) -> b) (iterate f (r,(ps,bs)))


go :: Int -> IO ()
go n = do
    xs <- run
    printf (""&"\n") (xs!!n)
-}

to_lst (x,y,z) = [x,y,z]

geom_seq = foldr (\xs n -> let r = foldr seq () xs 
                           in seq r (succ n)) 0 . map f . concatMap (\(G.LocGeom _ _ _ gp) -> gp)
 where f g =
        case g of
            G.Branch (Flat (r,g,b,a)) w1 w2 (x1,y1,z1) (x2,y2,z2) -> [r,g,b,a,w1,w2,x1,y1,z1,x2,y2,z2]
            G.Square (Tex s) h -> seq s [h]
            G.Li a b (c,d,e) (f,g,h) -> [a,b,c,d,e,f,g,h]
            G.Pt (a,b,c) (d,e,f) -> [a,b,c,d,e,f]
            G.PtA (a,b,c) (d,e,f) g s -> seq s [a,b,c,d,e,f,g]
            G.FnBranch _ xs (G.Fn f) -> concat [ (w:to_lst p) | (w,p) <- map f xs ]
 
get_geom = G.flatten . se_get_geom

--run2 :: StdGen -> Int -> Si2.MainStruct
run2 n = do
    sims <- LS.mk_plants
    let Just eng = lookup "lsystem" sims
    let ss = iterate se_simulate_step eng
    return $ ss!!n

go2 :: Int -> IO ()
go2 n = do
    st <- run2 n
    putStrLn (simrep_show st)

go3 :: Int -> IO ()
go3 n = do
    st <- run2 n
    mapM_ print (get_geom st)

time2 :: Int -> IO ()
time2 n = do
    st <- run2 n
    let s = geom_seq (get_geom st)
    printf ("done, "&" geomprims\n") s

povray :: Int -> IO ()
povray n = do
    st <- run2 n
    (writeFile "test.pov" . geom2povray (0,10,10) id_quat) (get_geom st)

ps = [('2',go2),('3',go3),('4',time2),('5',povray)]

main = do
    as <- getArgs
    case (as, map reads as) of
     ((['-',c]:_), [_,[(n,_)]]) ->
            maybe (return ()) ($n) (lookup c ps)
     _   -> return ()



