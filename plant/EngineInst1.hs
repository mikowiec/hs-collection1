
module EngineInst1 where


import PlantTypes
import Random
import SimEngine
import GeomRep
import ST
import Run
import Monad
import Misc
import MonadLib
import MonInter
import NonBasic

import DeepSeq

instance DeepSeq Tree

mk_sim_engine seed = do
    r <- newStdGen
    let tree = mk_tree seed newArr r
    
    return $ SimEngine $ SimEngineInst
        (\(_,tree) -> Geometry $ rd_geomrep $ draw tree)
        (\(_,tree) _ -> Geometry $ rd_geomrep $ draw tree)
        (\_ _ -> [])
        (\s _ _ -> s)
        fst
        (\_ -> (0,0,0))
        undefined
        (0,tree)
        (0,tree)
        (\(age,tree) _ -> (succ age, simul tree))
        (\_ -> StrTree 0 "" [])
        (\_ -> "<none>")

draw :: Tree -> RenderData
draw tree = runST (fst $^ runstp treeToRend tree)

simul :: Tree -> Tree
simul tree = runST (snd $^ runstp msimula tree)

