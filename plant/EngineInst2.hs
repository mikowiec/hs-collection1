
module EngineInst2 where

import Random
import GeomRep

import qualified Seed2
import qualified Simul2
import Draw2
import Property
import Misc
import Monad

import SimEngine

def_env = (SimEnv ())

mk_sim_engine :: Seed2.Seed -> IO SimEngine
mk_sim_engine seed = do
    let rnd = mkStdGen 7
    let sr = (Simul2.init_simul rnd seed)
    return $ SimEngine $ SimEngineInst
        draw_rep
        draw_rep_i
        find_struct
        update_struct
        get_age
        get_size
        def_env
        sr
        sr
        (\s e -> Simul2.simulate s)
        show_tree
        (show . Simul2.sims_map)

get_age s = Simul2.get_age s

get_size s = Simul2.get_size (Simul2.main_struct s)

draw_rep sims = Draw2.draw (Simul2.main_struct sims)

draw_rep_i sims sims' =
    Draw2.draw_interpolated (Simul2.main_struct sims)
                            (Simul2.main_struct sims')

show_struct = Simul2.showStruct

find_struct sims i =
    let smap = Simul2.sims_map sims
    in Simul2.prop_struct smap i

update_struct sims i p = 
    sims { Simul2.sims_map = Simul2.prop_update (Simul2.sims_map sims) i p }

show_tree sims =
    let smap = Simul2.sims_map sims
        show_tree' i = let str = Simul2.find_struct smap i
                       in StrTree i (show_struct str)
                                  (map (show_tree'.trd3) (Simul2.str_cs str))
    in show_tree' (Simul2.sims_root sims)
    

