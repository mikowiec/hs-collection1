
module Simul2 where

import LinAlg
import Types
import Monad
import Array
import Seed2
import MonadLib
import Pf
import Misc
import Random
import FiniteMap
import Maybe
import qualified ApproxTree as AT
import IdentMonad
import Property

import DeepSeq

type PartID = String

data Part = Part Code
 deriving (Eq, Ord)

instance DeepSeq Part where
    deepSeq (Part c) r =
        c `deepSeq` r

instance Show Part where
    show (Part c) = sprintf ("Part "&"") c

type StructID = AT.ID

data Structure = Structure {
    str_part :: Part,
    str_msg  :: [Message],
    str_cs   :: [(Position, Orientation, StructID)]
  } deriving Show


data FastKey = FSK [Message]
  deriving (Eq, Ord, Show)

data StructKey = SK Part [StructID]
  deriving (Eq, Ord, Show)



instance DeepSeq FastKey where
    FSK m `deepSeq` r = m `deepSeq` r


instance DeepSeq StructKey where
    SK h i `deepSeq` r = 
     h `deepSeq` i `deepSeq` r


{-
instance Eq StructKey where
    (SK Nothing _) == _ = False
    _ == (SK Nothing _) = False
    (SK a b) == (SK a' b') = a == a' && b == b'


instance Ord StructKey where
    (SK Nothing _) <= _ = False
    _ <= (SK Nothing _) = False
    (SK a b) <= (SK a' b')
        | a == a' = b <= b'
        | otherwise = a <= a'
-}


{-
instance Eq StructKey where
    SK (Just id) (Just inp) _ _ == SK (Just id') (Just inp') _ _ = id == id' && inp == inp'
    SK _ _ h st == SK _ _ h' st' = h == h' && st == st'
    _ == _ = False

instance Ord StructKey where
    SK (Just id) (Just inp) h st <= SK (Just id') (Just inp') h' st'
        = (id,inp,h,st) <= (id',inp',h',st')
    SK a b c d <= SK a' b' c' d' = (a,b,c,d) <= (a',b',c',d')
-}

type StructMap = AT.ApproxTree StructKey FastKey Structure
type MainStruct = (StructID, StructMap)

instance DeepSeq StdGen

instance DeepSeq Structure where
    deepSeq (Structure a b c) d = 
        a `deepSeq` b `deepSeq` c `deepSeq` d

find_struct map ix = case AT.find map ix of
                    Just s -> s
                    Nothing -> error (sprintf ("find_struct: "&"") ix)

prop_struct map ix = 
        let Part p = str_part $ find_struct map ix
        in (f p)
    where f (Code c) = to_prop (pst_state c)

prop_update map ix ps =
        AT.modify map ix f
 where f (Structure (Part p) m c) = Structure (Part (g p)) m c
       g (Code c) = Code (c { pst_state = (merge_prop ps (c.!pst_state))} )

showMainStruct n pos (root, map) = showStruct' n pos id_quat (find_struct map root)
 where
  showStruct' n pos ori (Structure part ms cs) =
     let indent = replicate n ' ' in
     showString indent .
      let cld | null cs   = showString ">\n"
              | otherwise = 
                    showString "\n" .
                     compMap (\(p,o,c) -> showStruct' (n+3) p o (find_struct map c)) cs .
                     showString (indent++">\n")
      in ssprintf ("<St pos:"&" ori:"&>q_fmt<&" part:"&" msg:"&" ")
            pos ori part ms . cld

showStruct (Structure part ms cs) =
      sprintf ("<St part:"&" ms:"&" cs:"&"") part ms cs
 

struct_fold :: StructMap -> (Structure -> a -> a) -> a -> StructID -> a
struct_fold smap f v root
 = let str = find_struct smap root
   in foldr (flip $ struct_fold smap f) (f str v) (map trd3 (str_cs str))

struct_foldM :: Monad m => StructMap -> (Structure -> a -> m a) -> a -> StructID -> m a
struct_foldM smap f v root = do
    let str = find_struct smap root
    v' <- f str v
    foldM (\v s -> struct_foldM smap f v s) v' (map trd3 (str_cs str))


get_size :: MainStruct -> (Int, Int, Int)
get_size (root, smap) = (a,b,c)
 where (a,b) = struct_fold smap f (0,0) root
       c = AT.sharing smap
       f (Structure _ _ []) (b,l) = (b,succ l)
       f _                  (b,l) = (succ b,l)


data SimS = SimState {
        sims_sseed :: Code,
        sims_rnd   :: StdGen,
        sims_id    :: Int,
        sims_root  :: StructID,
        sims_map   :: StructMap,
        sims_age   :: Int
    }

instance DeepSeq SimS where
    deepSeq (SimState s rn i ro m a) r =
     s `deepSeq` rn `deepSeq` i `deepSeq` ro `deepSeq` m
     `deepSeq` a `deepSeq` r

main_struct st = (sims_root st, sims_map st)

next_id :: Monad m => StateMonadP SimS m Int
next_id = do
    id <- sims_id $^ getst
    updst (\st -> (id, st { sims_id = succ id }))


type SimM a = StateMonad SimS a


init_simul :: StdGen -> Seed -> SimS
init_simul rnd (Seed code) =
    let init_state = SimState code rnd 1 rt AT.empty 0
        ((rt,_),s) = runst (new_code_struct code) init_state
    in s

new_code_struct c = do
    seed <- getstp sims_sseed
    rnd <- split_rnd
    let str = Structure (Part (init_code c)) [] []
    sim_key <- str_key Nothing str
    sid <- insert_st sim_key str
    return (sid,str)

map_smap :: (StructMap -> (StructMap,a)) -> SimM a
map_smap f =
    updst (\st -> let (m,r) = f (sims_map st)
                  in (r, st { sims_map = m }))

map_smap_ f = map_smap (\m -> (f m, ()))


cleanup = map_smap_ AT.cleanup

with_cleanup m = do
    r <- m
    cleanup
    return r

simulate :: SimS -> SimS
simulate st =
    snd $ runst (with_cleanup (sim_root (sims_root st))) st

sim_root rt = do
    rt' <- maybe (-1) id $^ simulate_step_ix rt
    modst (\st -> st { sims_root = rt',
                       sims_age = succ (st.!sims_age) } )


get_root :: SimS -> Structure
get_root sims =
    let (rt, smap) = (sims_root sims, sims_map sims)
    in find_struct smap rt

get_age ss = sims_age ss 

split_rnd = updst (\st -> let (r1,r2) = split $ st.!sims_rnd
                          in seq r1 $ seq r2 $ (r1, st { sims_rnd = r2 }))


find_st i = do
    st <- getst
    return (fromJust (AT.find (sims_map st) i))

insert_st key str =
    map_smap (\m -> AT.insert m key str)

update_st id (key,str) =
    map_smap (\m -> AT.update m id (FSK (str_msg str)) key str)

mprintf f = fmt (\str -> modst (\st -> seq st $ trace (fixup2 str) $ st)) f []
mprint s = mprintf (""&"") s

--simulate_step_ix :: StructID -> SimM (StructID, Structure)
simulate_step_ix i = seq i $ do
    str <- find_st i
    res <- simulate_step i str
    ifM res $ \(k,_,s) -> update_st i (k,s)


-- get_hash (Code s) = state_hash (pst_state s)

str_key sid str@(Structure part@(Part code@(Code p)) _ cs) = do
    let key = SK part (map trd3 cs)
    return key
    
run_code rnd code@(Code n) ms = do
    let run rs@(RunState n t) css ums dms = do
         case pst_cm_prog n of
          Nothing -> return (Just n, css, ums, dms)
          Just p -> do
           let (r, rs'@(RunState n' t')) = runst (run_ac (run_cm p)) rs
           case r of
             Nothing -> return (Just (n' { pst_cm_prog = Nothing }), css,ums,dms)
             Just (c,n) -> case c of
                Suspend i -> return (Just (n' { pst_cm_prog = Just (tell i (ContMonad (const n))) }), css,ums,dms)
                Die     -> return (Nothing,css,ums,dms)
                Message Up m   -> run rs' css (m:ums) dms
                Message Down m -> run rs' css ums (m:dms)
                SpawnCode pos ori m -> do
                    (i,s) <- new_code_struct m
                    run (RunState (n' { pst_cm_prog = (Just (action n)) }) t) (((pos,ori,i),s):css) ums dms
    
    (node',css,ums,dms) <- run (RunState n (Tmp rnd ms [])) [] [] []
    return (fmap Code node',css,ums,dms)


simulate_step :: StructID -> Structure -> SimM (Maybe (StructKey, [Message], Structure))
simulate_step sid str@(Structure part@(Part code@(Code p)) ms cs) = do
    children <- mapM (find_st.trd3) cs

    r <- split_rnd
    (code',new_css,ums,dms) <- {-# SCC "ss_run_code" #-} run_code r code ms
    let part' = fmap (\c -> Part c) code'

    let all_children = zip cs children ++ new_css
    let sub_sim ((pos,o,sid),str) = {-# SCC "ss_sub_sim" #-}
         do res <- simulate_step sid str
            ifM res $ \(key,ms,str) -> do
                    let str' = str { str_msg = str.!str_msg ++ dms }
                    sid' <- update_st sid (key,str')
                    return $ (ms, (pos,o,sid'))
    ifM part' $ \part' -> {-# SCC "ss_final_part" #-} do
        (ms,cs) <- (unzip . filterJust) $^ mapM sub_sim all_children
        let str = Structure part' (concat ms) cs
        sim_key <- str_key (Just sid) str
        return (sim_key, ums, str)




