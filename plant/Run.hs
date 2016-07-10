
module Run where

import Types
import PlantTypes
import GeomRep
import NonBasic
import MetaPlants
import MonInter
import LinAlg
import Random
import Draw
import MonadLib
import Misc(dbg)
import Array
import ST
import Data.Array.MArray
import Data.Array.ST
import STArrLib
import Misc
import NumExts
import BArrAtoms
import SimStateHelpers

-- import qualified LazyST
iterateST f s = do
        (v,s') <- f s
        vs <- unsafeInterleaveST $ iterateST f s'
        return (v:vs)

            
simulate :: TreeState s ()
simulate = do tree <- getst
              let st = tree_state tree
              msimula

{-
gen_reps :: IO [RenderData]
gen_reps = do r <- newStdGen
              print "starting to generate"
              let ret = runST (do barr <- newBArr
                                  xs <- iterateST simst (init_tree barr r)
                                  return xs)
              print "done generating"                                 
              return ret
              -}

gen_tree :: IO Tree
gen_tree = do
        r <- newStdGen
        let ret = init_tree newArr r
        return ret
              
newArr::IBArr
newArr = IBArr (array (0,0) [(0,null_state)]) 0 0
{-
gen_trees :: IO [GeomRep]
gen_trees = do r <- newStdGen
               let res = runST       (do barr <- newBArr
                                         (gerps, _) <- runstp (do seed <- getstp (st_seed.tree_state)

                                                                  ts <- simstp
                                                           
                                                                  let bss = map (head.st_bs.tree_state) ts
                                                                      reps = map2 Draw.draw bss (repeat seed)
                                                                  return reps) (init_tree barr r)
                                         return gerps)
               return res
-}
simst::Tree -> ST s (RenderData, Tree)
simst init_tree = do
        (_,tree) <- runstp simulate init_tree
        (rep,_) <- runstp treeToRend tree
        
        -- let bs = (head.st_bs.tree_state) tree
        --    rep = treeToRend tree -- bs (st_seed.tree_state $ tree)
        return (rep, tree)
               
treeToRend::TreeState s RenderData
treeToRend = do
        
        -- let bsi = (head.st_bs.tree_state) tree
        --     seed = (st_seed.tree_state) tree
        ist <- getstp tree_state

        cbs <- getstp (ist_cbs.tree_state)
       
        if (not $ null cbs) then (do let bsi = head cbs
                                     let (grep, _) = runst (Draw.draw bsi) ist

                                     let maxy = foldr max 0 $ map f grep
                                     let psize = maxy -- bs_dev $ bs
                                         zback = (floatToDouble psize) + 0.0
                                         yup = (floatToDouble $ psize / 2.0)
                                         
                                     return $ RenDat grep (0,yup,zback) (0,yup,0))
                            else return null_render
               
        where f (Li _ _ (a,b,c) (d,e,f)) = foldr max 0 [a,b,c,d,e,f]
              f _ = 0
              null_render = RenDat [] (0,0,0) (0,0,0)
        
simstp::TreeState s [Tree]
simstp = do 
           st <- getst
           sts <- simstp
           return (st:sts)
        

--------------------------------------
iodraw::Tree -> IO RenderData
iodraw tree = do
        let rendat = runST (do (rendat,_) <- runstp treeToRend tree
                               return rendat)
        return rendat
        
iosim::Tree -> IO Tree
iosim tree = do
        let ntree = runST (do (_,ntree) <- runstp msimula tree
                              return ntree)
        return ntree 

ioclone::Tree -> IO Tree
ioclone tree = do
        let ntree = runST (do (_,ntree) <- runstp mclone tree
                              return ntree)
        return ntree 



mk_tree :: Seed -> IBArr -> Rnd -> Tree
mk_tree seed arr r = 
                   let flags = array (FeedLeaves, EmptyFlag) [(FeedLeaves, True), (EmptyFlag, False)]
                       init_mum = []
                       istate = (ISt r seed null_env 0 0.0 flags [] [] arr init_mum 0.01)
                   in (Tree siml Nothing istate)

init_tree :: IBArr -> Rnd -> Tree
init_tree arr r = mk_tree tt arr r

---------------------------------------

season p t 0 = []
season p t n = (p,t):(season p t (n - 1))
siml :: Simlist
siml = (season spring 0.083 3) ++ (season summer 0.083 3) ++ (season fall 0.083 3) ++ (season winter 0.083 3) ++ siml

spring = [("water", 0.5)]
summer = [("water", 2.5)]
fall   = [("water", 0.1)]
winter = [("water", 0.0)]

-- run_draw :: Tree -> (ProcGeomRep -> IO (GeomRep,GeomRep)) -> IO Tree
{-
run_draw tree rep draw_fn = do
    let -- rep = tree_rep tree
        st = tree_state tree
        s = st_seed st
        emptybs = let ret = (null (st_bs st))
                  in ret -- dbg (show ret) $ ret

    let rep' = case (rep, emptybs) of
                (Nothing, False) -> let b = head $ st_bs st
                                    in Left (Draw.draw b s)
                (Nothing, True) -> Left []
                (Just rep, _)   -> Right rep

    rep'' <- draw_fn rep'
    return (Just rep'')
-}



