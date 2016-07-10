
module Seed2 where

import Array
import Types
import MonadLib
import Monad
import LinAlg
import Pf
import Misc
import Random
import GeomRep
import List
import Property

import DeepSeq

import IdentMonad

-- import FuncExpr

type Power = Float
type Position = Float
type Orientation = Quat
type Mass = Float
type EMod = Float
type Devel = Float
type Pos   = Float

type PowerDist pwr = pwr -> (pwr, [pwr]) -> (pwr, [pwr])

type Message = String

data Dir = Up | Down
 deriving Show

data ApexCmd = SpawnCode Position Orientation Code
             | Die
             | Message Dir Message
             | Suspend Int

instance Show ApexCmd where
    show (SpawnCode p o _) = sprintf ("Spawn pos:"&" ori:"&"") p o
    show Die = "die"
    show (Message d m) = sprintf ("msg "&" "&"") d m
    show (Suspend i) = sprintf ("suspend "&"") i

data Action m a
 = Atom (m (Action m a))
 | Action ApexCmd (m (Action m a))
 | Par [Action m a] (m (Action m a))
 | End

instance Show (Action m a) where
    show End = "end"
    show (Atom _) = "atom"
    show (Action _ _) = "action"
    show (Par xs r) = "par " ++ show xs

action m = lft $ ContMonad (\_ -> m)
atom m = lft $ ContMonad (\k -> Atom (m >>= \a -> return (k a)))
suspend :: Monad a => IdentMonad (ContMonad (Action a c)) ()
suspend = 
 let m = lft $ ContMonad $ \k -> Action (Suspend i) (return (k ()))
     i = im_id m
 in m

nxt (IM (Ref (i, m))) = 
 let m' = unCM m (\_->End) in
 lft $ ContMonad $ \_ -> Action (Suspend i) (return m')

is_susp (Suspend _) = True
is_susp _ = False

run_ac (Atom m) = m >>= run_ac
run_ac (Action c m) = m >>= \a -> return $ Just (c,a)
run_ac End = return Nothing

run_ac (Par [] m) = m >>= run_ac

run_ac (Par xs n) = do
    rs <- filterJust $^ mapM run_ac xs
    let (ss,rs') = partition (is_susp . fst) rs
    let cs = map fst rs'
        ms = map snd rs'
    foo cs ms (map snd ss) >>= run_ac
 where foo [] ms ss = return (Action (Suspend (im_id (lft (return ()::[()])))) (return (Par (ss++ms) n)))
       foo (c:cs) ms ss = return (Action c (foo cs ms ss))


code :: IdentMonad (ContMonad (Action (StateMonadP (RunState s) IdMonad) a)) ()
code = do
    RunState _ _ <- atom getst
    suspend
    return ()

run_cm :: IdentMonad (ContMonad (Action m a)) a -> Action m a
run_cm c = unCM (run_im c) (\_-> End)

weave m n = par [m,n]

par ms = lft $ ContMonad $ \k ->
    Par (map run_cm ms) (return $ k ())

{-
loop m = do
    m
    suspend
    loop m
-}

loop m = 
    let loop'' = m >> nxt loop''
    in loop''

test_run p = 
    let run p = do r <- run_ac p
                   case r of
                    Nothing -> return End
                    Just (c,r) -> do
                        run r
                                
    in
    let (r,s) = runst (run (run_cm p)) (RunState (init_node undefined ["start"]) undefined)
    in pst_state (ps_part s)


-- The persistent data part of a node ..
data Node s = Node {
        pst_shape      :: Shape s,
        pst_geom       :: PartGeom,
        pst_pos        :: Pos,
        pst_cm_prog    :: Maybe (IdentMonad (ContMonad (Action (StateMonadP (RunState s) IdMonad) ())) ()),
        pst_state      :: s
  }

node_cmp :: Node a -> Node b -> Ordering
node_cmp (Node _ _ p c _) (Node _ _ p' c' _) =
    case p `compare` p' of
     EQ -> fmap im_id c `compare` fmap im_id c'
     o  -> o

instance Eq (Node s) where
    n == n' = True

instance Ord (Node s) where
    n `compare` n' = EQ

instance DeepSeq (Node s) where
    (Node s g p c st) `deepSeq` r = 
        s `seq` g `seq` p `deepSeq` c `seq` r

-- .. and the temporary part
-- (could be split in two, and having reader resp. writer monads)
data Tmp = Tmp {
        pst_rnd        :: StdGen,
        pst_msg        :: [Message],
        pst_cmd        :: [ApexCmd]
  }

instance DeepSeq Code where
    deepSeq (Code n) r = 
        n `deepSeq` r

-- A program is run on the persistent state and a temporary state
data RunState s = RunState { ps_part :: Node s, ps_tmp :: Tmp }


-- Signatures of a program
type SM_ s  = IdentMonad (ContMonad (Action (StateMonadP (RunState s) IdMonad) ())) ()
type SM s a = IdentMonad (ContMonad (Action (StateMonadP (RunState s) IdMonad) ())) a

instance Show (Node s) where
    show _ = ""
{-
instance Show s => Show (PartState s) where
    showsPrec _ ps =
        ssprintf ("<PS pos:"&" lc:"&">") (pst_pos ps) (pst_state ps)

instance Show PState where
    showsPrec _ (PState ps ts) =
        ssprintf ("<AS cmd:"&" length:"&">")
           (pst_cmd ts) (pst_pos ps)
-}

init_node m s = Node (\_ _ p->(0.1,y_pos p)) (\_ _->[]) 0 m s

-- init_partstate s = PartState (\p -> (0.1,y_pos p)) (\_ _ _ _ -> []) 0 s


-- Pos must be visible since it's used as a parameter
-- when geometrifying the shape.
type GravityDir = Vector
type Shape s = s -> GravityDir -> Pos -> (Float,Vector)

type ExtShape = GravityDir -> Pos -> (Float,Vector)
type ExtGeom = GravityDir -> Pos -> GeomRep

-- Geometry in the interval [0..pos]
type PartGeom = (Pos -> (Float,Vector)) -> Pos -> GeomRep


y_pos y = (0,y,0)

ahead _ = rot_axis (0,0,-1) 0


width_fn a l = 0.01 * (fromIntegral a - l/0.1)

type Prob = Float

class (Eq s, Ord s, PropRep s, Show s) => NodeState s where
    node_init :: s

instance PropRep () where
    to_prop _ = []
    merge_prop _ _ = ()

instance NodeState () where
    node_init = ()

-- A piece of program, including its current state
data Code = forall s . (NodeState s) => Code (Node s)

instance Eq Code where
    Code c == Code c' = node_cmp c c' == EQ

instance Ord Code where
    Code c `compare` Code c' = node_cmp c c'

instance Show Code where
    show (Code s) = sprintf ("<"&">") s


--coerse (Code p m) = Code p m

--get_pstate :: (forall s . PCode s) -> Node s'
--get_pstate (Code (Node s g p _) _) = Node s g p undefined
--set_pstate (Code (Node _ _ _ s) m) n = Code (n { pst_state = s}) m

init_code :: Code -> Code
init_code (Code (Node _ _ _ m s)) = Code (init_node m s)

{-
run_code :: StdGen -> Code -> [Message] -> (Code,[ApexCmd])
run_code rnd code@(Code n) ms = 
    let (_, RunState n' ts') = runst (pst_prog n) (RunState n (Tmp rnd ms []))
    in (Code n', pst_cmd ts')
-}


{-
run_code_with :: StdGen -> Code -> StateMonad (PState,()) a -> (Code,[ApexCmd])
run_code_with rnd code@(Code p s m) before = 
    let (r1, (p',_))   = runst before (PState p (TmpState rnd [] []),())
        (_, (PState p'' ts,s')) = runst m (p',s)
    in (Code p'' s' m,pst_cmd ts)
-}

get_pst :: StateMonad (RunState s) (RunState s)
get_pst = getst

get_pst_f f = f $^ get_pst

get_lst :: StateMonad (RunState s) s
get_lst = getstp (pst_state.ps_part)
get_lst_f f = f $^ get_lst

get_rnd_hlp rnd_fn sel upd = do
        updst (\s ->
            let (f,r2) = rnd_fn (sel (ps_tmp s))
            in (f, s { ps_tmp = upd (ps_tmp s) r2 }))

pt_hlp f = get_rnd_hlp f pst_rnd (\st r -> st { pst_rnd = r })

instance Monad m => RndGen (StateMonadP (RunState s) m) where
    rnd  = pt_hlp norm_rnd
    rnds = pt_hlp norm_rnds


data Seed = Seed Code

instance Show Seed where show _ = "<seed>"


-- dummy_seed = mk_seed (\_-> setst (1,[dummy_code]))


mk_seed prog = Code prog
{-
  let as = (snd.snd) (runst (mfix prog) (0,[]))
  in Seed 0 (mk_arr as)
-}

mk_arr [] = error "Empty programs not allowed!"
mk_arr xs = let len1 = length xs - 1 in
    array (0,len1) (zip [len1,len1-1..] xs)

modpst f = modst (\st -> f st)


push s = atom $ modlst (s:)


modlst f = modst (\(RunState st t) -> RunState (st { pst_state = f (st.!pst_state) }) t )

class PowerC a where
    power :: a -> Power
    upd_power :: a -> Power -> a
    
{-
gen_power pw = do
    modlst (\st -> upd_power st (pw + power st))
    -}

{-
modpsst f = modpst (\(PState p t) -> PState (f p) t)
modtst f = modpst (\(PState p t) -> PState p (f t))
-}

set_geom g    = atom $ modpst (\(RunState st ta) -> RunState (st { pst_geom = g }) ta)
set_shape f   = atom $ modpst (\(RunState st ta) -> RunState (st { pst_shape = f }) ta)
--set_thickness f = modpst (\st -> st { pst_thickness = f })
-- set_power_dist ps = modpst (\st -> st { pst_power_dist = ps })

set_lstate s = modst (\(p,_) -> (p,s))

get_power :: PowerC s => StateMonad (RunState s) Power
get_power = getstp (power.pst_state.ps_part)

{-
set_power pw = modlst (\st -> upd_power st pw)

use_power pw = get_power >>= set_power . (\p -> p - pw)
-}

-- get_power_dist :: StateMonad (PState,s) [Power]
-- get_power_dist = getstp (\(p,_) -> pst_power_dist p)

--get_density :: StateMonad (PState,s) Float
--get_density = getstp (pst_density.fst)

{-
get_length :: StateMonad (RunState s) Float
get_length = getstp (pst_pos.ps_part.fst)
-}

push_cmd cmd = do
    modpst (\(RunState pa st) -> RunState pa $ st { pst_cmd = cmd : pst_cmd st })

get_cmd :: StateMonad (RunState s) [ApexCmd]
get_cmd = getstp (pst_cmd.ps_tmp)

reset_cmd :: StateMonad (RunState s) ()
reset_cmd = modst (\(RunState pa st) -> (RunState pa $ st { pst_cmd = [] }))

mk_leaf s sh len =
    [Square s (fst (sh len))]

mk_surface s l r m t sh len =
    [Surface s l r m t]

mk_branch s _ len | len < 0.001 = []
mk_branch s sh len = 
    let 
        fa d = fst (sh d)
        pos d = snd (sh d)
    in [ Branch s (fa 0) (fa len) (pos 0) (pos len) ]

mk_bendy_branch s _  len | len < 0.001 = []
mk_bendy_branch s sh len  = 
    let 
        fa d = fst (sh d)
        pos d = snd (sh d)
    in [ FnBranch s [0,len] (Fn (\p -> (fa p, pos p))) ]

--range l h v = max l $ min h $ v

{-
spawn_code o m = do
    (p,rs) <- atom $ do
        p <- get_pst_f (pst_pos.ps_part)
        rs <- rnds
        return (p,rs)
    lft $ ContMonad (\k->Action (SpawnCode p (o rs) m) (return (k ())))
-}

{-
spawn m = spawn_ori (QConst id_quat) m

spawn_ori o m = do
    (p,rs,ps) <- atom $ do
        p <- get_pst_f (pst_pos.ps_part)
        ps <- get_pst_f (to_prop.pst_state.ps_part)
        rs <- rnds
        return (p,rs,ps)
    let q = run qeval o rs (0,-1,0) 0 ps
    lft $ ContMonad (\k->Action (SpawnCode p q (Code (init_node (Just m) node_init))) (return (k ())))
-}

{-
spawn_code'' o f m = do
    (p,rs) <- atom $ do
        p <- get_pst_f (pst_pos.ps_part)
        rs <- rnds
        return (p,rs)
    lft $ ContMonad (\k->Action (SpawnCode p (o rs) (Code (init_node (Just (nxt m)) (f node_init)))) (return (k ())))
-}

seed m = Seed (spec m)

{-
replace_prog m = do
    modst (\(RunState p t) -> RunState (p { pst_prog = m }) t)
-}

--spec :: (Show s, LocalState s) => String -> StateMonad (PState, s) a -> (String, PCode a)
spec m = spec' node_init (Just m)

--spec' :: String -> s -> StateMonad (RunState s) () -> (String, Code)
spec' i m = Code (init_node m i)

-- spec' l m = (l, Code undefined (spec_init m) (spec_m m))


set_pos l = atom $ modpst (\(RunState st ta) -> RunState (st { pst_pos = l }) ta)

get_pos :: SM s Float
get_pos = atom $ getstp (pst_pos.ps_part)

instance PropRep Int where
    to_prop i = [("int",PropInt i)]
    merge_prop [(_,PropInt i)] _ = i
    merge_prop _ p = p

{-
init_sim' init sim = 
	(\m -> init >> m) . (replace_prog . sim g)
   where g = init_sim' init sim


init_sim init sim = 
	  init >> (replace_prog $ sim g)
   where g = init_sim init sim

stop :: (Show s, LocalState s) => StateMonad (RunState s) ()
stop = replace_prog (return ())
-}

get_c_pos (Code pst) = pst_pos pst
get_c_geom (Code pst) g =
    (pst_geom pst) ((pst_shape pst) (pst_state pst) g)
get_c_shape (Code pst) = pst_shape pst (pst_state pst)

--getlst :: SM s s
--getlst = (pst_state.ps_part) $^ getst
{-
modlst f = modst (\s -> s { ps_part =
    (ps_part s) { pst_state = f (pst_state (ps_part s)) } }  )
-}

