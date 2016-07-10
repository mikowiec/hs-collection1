
module LSystem where

import Monad
import Seed2
import Types
import LinAlg
import MonadLib
import Misc
import EngineInst2
import IdentMonad
import Property
import SimEngine
-- import FuncExpr

data LSysParams = LSys {
    a1 :: Float,
    a2 :: Float,
    p1  :: Float,
    p2  :: Float,
    w0  :: Float,
    min_s  :: Float,
    r1  :: Float,
    r2  :: Float,
    q  :: Float,
    s  :: Float,
    w  :: Float,
    cs :: Float
  }
 deriving (Show, Read, Eq, Ord)


{-
instance LocalState LSysParams where
    state_init = tree2
    state_hash ls = Just [round (s ls + 1000 * w ls)]
-}

instance PropRep LSysParams where
    to_prop (LSys a1 a2 p1 p2 w0 min_s r1 r2 q s w cs) = 
        [("s", PropFloat s), ("w", PropFloat w), ("cs", PropFloat cs)]
    merge_prop ps ls = foldr f ls ps
     where f p l = case p of
                    ("s", PropFloat f) -> l { s = f }
                    ("w", PropFloat f) -> l { w = f }
                    ("cs", PropFloat f) -> l { cs = f }

instance NodeState LSysParams 
--    node_init = tree2

classic   = LSys (d2r 35) (d2r (-35))   0   0 0.3 0 0.75 0.77 0.5 1 0.3 0
tree      = LSys (d2r 30) (d2r (-30)) 137 137 0.3 0 0.8 0.8 0.5 1 0.3 0
test      = LSys (d2r 90) (d2r (-90))   0   0 0.3 0 0.75 0.75 0.5 1 0.3 0
tree2     = LSys (d2r 10) (d2r (-40)) 137 137 0.3 0 0.74 0.76 0.5 1 0.3 0

stop _ = lft $ ContMonad (\k -> End)


plant_lsystem = do
       w <- w $^ (atom get_lst)
       set_geom (mk_bendy_branch (Flat (0.23,0.58,0.15,1)))
       set_shape (\_ d k -> (max w 0.01, ((k*0.6) `mulk` d) `add` (0,2*k,0)))
       loop $ do
         LSys a1 a2 p1 p2 w0 min_s r1 r2 q s w cs <- atom get_lst
         case (cs < s, s <= min_s) of
          (_, True) -> stop ()
          (True, _) -> do
               atom $ modlst (\s -> s { cs = cs + 0.02 })
               set_pos cs
          _ -> do
             set_pos s
             {-
	     let child a p r q = spawn_ori (QConst $ rot_axis (0,0,1) a `mulq` rot_axis (0,1,0) p)
                                            (do atom $ modlst (\p->tree2 { s = r*s, w = q*w })
                                                nxt plant_lsystem)
             child a1 p1 r1 q
             child a2 p2 r2 (1-q)
             -}
             stop ()

plant_lsystem_init = do
    atom $ modlst (const tree2)
    nxt plant_lsystem


mk_plants :: IO [(String, SimEngine)]
mk_plants = sequence (
        liftM ((,) "lsystem" ) (mk_sim_engine (seed plant_lsystem)) : 
        liftM ((,) "lsystem_init" ) (mk_sim_engine (seed plant_lsystem_init)) : 
    [])


