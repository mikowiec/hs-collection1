
module Plants1 where

import Monad
import Seed2
import EngineInst2
import WrapParam
import MetaPlants

plant_betula = parametric_plant betula

mk_plants = sequence (
        liftM ((,) "betula" ) (mk_sim_engine (seed plant_betula)) : 
    [])


