
module WrapParam where

import Monad
import Seed2
import Types
import LinAlg
import MonadLib
import Misc
import Random

import Run
import MetaPlants
import EngineInst1
import EngineInst2
import NonBasic
import GeomRep
import PlantTypes
import Property

instance Eq Tree where
    _ == _ = False

instance Ord Tree where
    _ < _ = True

instance PropRep Tree where
    to_prop = std_to_prop
    merge_prop _ p = p

instance Show Tree where
    show _ = "<tree>"

instance NodeState Tree where
    node_init = undefined

parametric_plant seed = (c::SM_ Tree)
 where
  c = do
   atom $ modlst (\_->mk_tree seed newArr (mkStdGen 2))
   loop $ do
    t <- atom get_lst
    set_geom (\_ _ -> rd_geomrep $ draw t)
    atom $ modlst simul
 


