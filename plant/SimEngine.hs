
module SimEngine where

import GeomRep
import Types
import Property

import Misc
import Monad
import Maybe

import DeepSeq

type NodeID = Int
data StrTree = StrTree NodeID String [StrTree]

data SimEnv = SimEnv {
    lightbox :: ()
  }

data SimEngineInst simrep = SimEngineInst {
    se_draw :: (simrep -> GNode),
    se_draw_i :: (simrep -> simrep -> GNode),
    se_find :: (simrep -> NodeID -> Properties),
    se_update :: simrep -> NodeID -> Properties -> simrep,
    se_age  :: (simrep -> Int),
    se_size :: (simrep -> (Int,Int,Int)),
    se_env  :: SimEnv,
    se_last_rep :: simrep,
    se_simrep :: simrep,
    se_step :: (simrep -> SimEnv -> simrep),
    se_strtree :: (simrep -> StrTree),
    se_show :: (simrep -> String)
  }

data SimEngine = forall s . DeepSeq s => SimEngine (SimEngineInst s)

instance DeepSeq s => DeepSeq (SimEngineInst s) where
    (SimEngineInst _ _ _ _ _ _ _ _ e _ _ _) `deepSeq` r =
        e `deepSeq` r

instance DeepSeq SimEngine where
    SimEngine i `deepSeq` r = i `deepSeq` r
 
se_get_age (SimEngine s) = se_age s (se_simrep s)
se_get_size (SimEngine s) = se_size s (se_simrep s)

se_simulate_step (SimEngine eng) =
    let sr = se_simrep eng
        sr' = se_step eng sr (se_env eng)
    in sr `deepSeq` sr' `deepSeq` SimEngine (eng { se_last_rep = sr, se_simrep = sr' } )

{-
se_fold_geom (SimEngine eng) i f = (i', SimEngine (eng { se_env = env' }))
 where (i',env') = se_draw eng (se_simrep eng) (i,se_env eng) g
       g ix gp (a,env) = let a' = f ix gp a in (a',env)
-}

se_fold_geom (SimEngine eng) i f =
    (f 0 (se_draw eng (se_simrep eng)) i, SimEngine eng)

se_get_geom (SimEngine eng) =
    se_draw eng (se_simrep eng)

se_set_env (SimEngine eng) f1 =
    SimEngine (eng { se_env = (f1 ((se_env eng))) })

se_get_geom2 (SimEngine eng) =
    se_draw_i eng (se_last_rep eng) (se_simrep eng)
-- fst $ se_fold_geom eng [] (\_ gp -> (gp++))

se_find_struct (SimEngine eng) ix =
    let sr = se_simrep eng in
    se_find eng sr ix

se_update_struct (SimEngine eng) ix ps =
    let sr = se_simrep eng in
    SimEngine (eng { se_simrep = se_update eng sr ix ps })

simrep_strtree (SimEngine eng) = 
    let sr = se_simrep eng in
    se_strtree eng sr

simrep_show (SimEngine eng) =
    (se_show eng) (se_simrep eng)



