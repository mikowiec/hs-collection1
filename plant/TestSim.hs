

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

import Monad
import System

import NoLoadModule as LM
import DeepSeq

instance DeepSeq Surface where
    Flat c `deepSeq` r = c `deepSeq` r
    Tex t `deepSeq` r = t `deepSeq` r
    TexBlend p s s' `deepSeq` r = (p,s,s') `deepSeq` r

instance DeepSeq G.GeomPrim where
    g `deepSeq` r = 
         case g of
          G.Li a a' p p' -> (a,a',p,p') `deepSeq` r
          G.LiA w w' p p' c b -> (w,w',p,p',c,b) `deepSeq` r
          G.Pt p v -> (p,v) `deepSeq` r
          G.PtA p v n s -> (p,v,n,s) `deepSeq` r
          G.Branch s w w' p p' -> (s,w,w',p,p') `deepSeq` r
          G.Square s n -> (s,n) `deepSeq` r
          G.FnBranch s pt (G.Fn f) -> (map f pt) `deepSeq` r

all_mods =  ["Seed2", "Simul2", "Draw2", "EngineInst2"] ++
            ["NonBasic", "STArrLib", "BArrAtoms", "Helpers", 
             "SimStateHelpers", "SimStateIHelpers", "Draw", 
             "MetaLib", "Meta", "MetaPlants", "MonInter", 
             "Run", "EngineInst1", "Wither", "MetaLibHigh", "MetaLibLow"]

load_systems mod = do
    LM.init_fg
    mapM_ LM.load_obj all_mods
    LM.compile_fg mod mod
    Just h <- LM.load_obj mod
    mk_sim <- LM.load_sym h (mod ++ "_mkzuplants_closure")
    sims <- mk_sim
    return sims


 
get_geom  = G.flatten . G.apply_param (0,(0,-1,0)) . se_get_geom
get_geom2 p = G.flatten . G.apply_param (p,(0,-1,0)) . se_get_geom2

povray p (s,ps) = do
    let pv = geom2povray (3.29,4.97,5.49) ((0.31,-0.25,-0.09), 0.9) (get_geom2 p s)
    case ps of
     [] -> putStr pv
     (fn:_) -> writeFile fn pv

flat_geom (s,ps) = do
    print (G.flatten (se_get_geom s))


geom (s,ps) = do
    print (G.apply_param (0,(0,-1,0)) (se_get_geom s))

geom2 (s,ps) = do
    print (G.apply_param (1,(0,-1,0)) (se_get_geom2 s))

run_povray fn = do
    let w = 720
        h = 576
        out = (remove_ext fn) ++ ".ppm"
    system (sprintf ("povray +A +FP +W"&" +H"&" +I"&." +O"&."") w h fn out)

convert fn = do
    system (sprintf ("convert "&.".png "&.".jpeg") fn fn)

prof (s,ps) = 
  printf ("number of primitives: "&"\n") n
 where
  n = foldr f 0 (G.flatten (se_get_geom s))
  f (G.LocGeom v o so i gr) n = (v,o,so,i,gr) `deepSeq` (n + length gr)

mpeg (ss, (fn:from:to:_)) = do
    let [from',to'] = map (read::String->Int) [from,to]
    let n = to' - from'
    let ss' = take n (drop (pred from') ss)
    let fs = map (\i -> fn ++ int_fill '0' 4 i) [0..n-1]
    let pm = 0
    mapM_ (\(s,f) -> povray pm (s,[f])) (zip ss' fs)
    mapM_ (run_povray.(++".pov")) fs
--    mapM_ convert fs
    system (sprintf ("sampeg -f ppm -2 -i "&."%04d.ppm -o "&.".mpeg") fn fn)
    return ()

sim_n (ss, []) = (head ss, [])
sim_n (ss, (n:ps)) = (ss!!(read n), ps)

subs = 
 ("test", printf ("->"&"\n") . simrep_show . fst . sim_n) :
 ("geom", geom . sim_n) :
 ("geom2", geom2 . sim_n) :
 ("flat_geom", flat_geom . sim_n) : 
 ("povray", povray 0 . sim_n) :
 ("mpeg", mpeg) :
 ("prof", prof . sim_n) :
 []

run s f params = do
    let ss = iterate' se_simulate_step s
    f (ss, params)

main = do
    (sub:modmodel:params) <- getArgs
    let (mod,model) = break (==':') modmodel
    ss <- load_systems mod
    Just f <- return $ lookup sub subs
    let s = maybe (snd $ head ss) id (lookup (dropWhile (==':') model) ss)
    run s f params
 `catch` \_->usage


usage = do
    fn <- getProgName
    printf ("usage: "&." <sub> <model> <...>\n") fn

