

module Simple where

import Monad
import Seed2
import Types
import LinAlg
import MonadLib
import Misc
import EngineInst2



plant_simple = c


c :: SM_ ()
c = do
   set_shape (\_ _ _ -> (0.1,(0,1,0)))
   loop $ (get_pos >>= set_pos . succ) `weave` do
     f <- (/50) $^ get_pos
     set_geom $ mk_leaf $
      if f<1 then TexBlend f "leaf1" "leaf_yellow"
          else TexBlend (f-1) "leaf_yellow" "leaf_brown"

plant_simple2 = (c2::SM_ ())
 where
  c2 = do
    set_shape (\_ _ p -> (0.1, (p/10,0,0)))
    --set_pos 1
--    spawn plant_simple
  --  set_pos 2
--    spawn plant_simple


mk_plants = sequence (
        liftM ((,) "simple" ) (mk_sim_engine (seed plant_simple)) : 
        liftM ((,) "simple2" ) (mk_sim_engine (seed plant_simple2)) : 
    [])


