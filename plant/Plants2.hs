module Plants2 where

import Monad
import LinAlg
import Seed2
import Misc
import MonadLib
import Types
import EngineInst2
import Pf
import Property
import FiniteMap

branch_ori       (k:l:_) = rot_axis (0,1,0) (2*k*pi) `mulq` rot_axis (0,0,-1) (l*pi/8 + pi/8 + pi/2)
small_branch_ori (k:_)   = rot_axis (1,0,0) (k/10 + pi/3)
leaf_ori         (k:_)   = rot_axis (0,0,1) (2*k*pi)
iroot_ori        (k:_)   = rot_axis (0,0,-1) (pi+ (k-0.5)/4)
root_ori         (k:l:_) = rot_axis (0,1,0) (2*k*pi) `mulq` rot_axis (0,0,-1) (l*pi/2)
leaf_ori'        (k:_)   = rot_axis (0,0,1) (pi/2)
leaf_ori'' i     (k:l:m:_) = rot_axis (0,1,0) ((l-0.5)*0.4 + i*pi) `mulq`
                             rot_axis (1,0,0) ((k-0.5)*0.4 + pi/3)
br_ori' i (k:l:_) = rot_axis (0,1,0) (i*pi/3 + l) `mulq` 
     rot_axis (0,0,-1) (pi/6 + k/3)

ori_down = rot_axis (0,0,-1) pi
ori_y k = rot_axis (0,1,0) (2*k*pi)

{-
-- plant_granskog = 
    let forest = do
            set_geom (mk_bendy_branch (Flat (1,1,1,1)))
--            set_density 0
--            set_thickness (\_ _ -> 0.01)
            set_shape (\k -> (0.002, (1 * fromInteger (round k `mod` 3),0,1*(fromInteger $ floor $ k/3))))
            mapM (\i -> do
                set_pos i
                spawn_code ahead (spec "stem" stem)
                ) [0..0]
            replace_prog tick_devel

        stem = do
            set_density 0
            set_geom (mk_bendy_branch (Flat (0.03,0.03,0.01,1)))
            set_growth 0.05
--            set_thickness (\a l -> range 0.01 0.1 ((fromIntegral a)*0.001 - l*0.03))
            spawn_code (\(k:_) -> ori_y k `mulq` ori_down) (spec "root" root)
            replace_prog $ do
                with_devel $ \d -> 
                    set_shape (\k -> (fromIntegral d / 1000.0, (0,k,0)))
                std_prog
                gen_power 1
                mapM_ (\i -> every 5 (spawn_code (br_ori' i) (spec "branch" branch))) [0..5]
                at 50 $ set_growth 0

        branch = do
            std_prog
            set_geom (mk_bendy_branch (Flat (0.1,0.35,0.05,1)))

            gen_power 2
            let leafx2 = do
                  spawn_code (leaf_ori'' 0) (spec "leaf" leaf)
                  spawn_code (leaf_ori'' 1) (spec "leaf" leaf)
            at 1 leafx2
            every 5 leafx2

            set_density 0.8
            set_growth 0.010
            with_devel $ \d -> 
                set_shape (\k -> (0.01 `min` (d/10000), (-k*sin (k/10),k,0)))
            set_emod 0.7
            at 20 $ stop

        leaf = do
            std_prog
            set_geom (mk_leaf (Tex "tuja"))
            gen_power 3
            set_density 0
            set_growth 0.0050
            when_devel (==10) $ set_growth 0 >> replace_prog std_prog

        root = do
            set_geom (mk_bendy_branch (Flat (0.1,0.1,0.05,1)))
            set_growth 0.005
            let sim = do
                with_devel $ \d ->
                    set_shape (\k -> let r = 2*k in (0.02 `min` (d / 1000), k `mulk` (sin (2*r), 1, sin r)))
                gen_power 2
                std_prog
                every 10 $ spawn_code root_ori (spec "root" root)
            sim
            replace_prog sim
    in spec "forest" (forest :: StateMonad (PState,LState) ())
-}

instance NodeState Dev where
    node_init = Dev 0

with_power f = do
    p <- get_power
    f p

data Nil = Nil
 deriving (Eq,Ord)

data TL h t = TL h t
 deriving (Eq,Ord)

instance (PropRep h, PropRep t) => PropRep (TL h t) where
    to_prop (TL h t) = to_prop h ++ to_prop t
    merge_prop ps (TL h t) = TL (merge_prop ps h) (merge_prop ps t)

instance Show Nil where
    showsPrec _ Nil = id

instance (Show h, Show t) => Show (TL h t) where
    showsPrec _ (TL h t) = shows h . showString " " . shows t

tl_lift f (TL h t) m = let (t',r) = f t m in (TL h t', r)

class Val v a | v -> a where
    val :: v -> a
    setv :: v -> a -> v


type PropMap = FiniteMap String PropType
data Vars = Vars PropMap

instance Eq Vars where
    _ == _ = False

instance Ord Vars where
    _ < _ = False

instance Show Vars where
    show _ = ""

instance PropRep Vars where
    to_prop _ = []
    merge_prop _ s = s

instance NodeState Vars where
    node_init = Vars emptyFM

instance Val Vars PropMap where
    val (Vars fm) = fm 
    setv (Vars _) fm = Vars fm

class PropVal a where
    val_of_prop :: PropType -> a
    prop_of_val :: a -> PropType


instance PropVal Vector where
    val_of_prop (PropVector v) = v
    prop_of_val v = PropVector v

instance PropVal Float where
    val_of_prop (PropFloat f) = f
    prop_of_val f = PropFloat f
  
decl var val = do
    map_val (\fm -> addToFM fm var (prop_of_val val))

get var = do
    fm <- val $^ get_lst
    case val_of_prop `fmap` lookupFM fm var of
     Just v -> return v

tl_upd t@(TL hd tl) Nothing = (t, val hd)
tl_upd t@(TL hd tl) (Just v) = (TL (setv hd v) tl, v)

instance Val v a => Val (TL v t) a where
    val (TL h _) = val h
    setv (TL h t) v = TL (setv h v) t

map_val f = do
    modlst (\s -> setv s (f (val s)))

lift_sub m = do
    (p, TL h t) <- getst
    let (_,(p',t')) = runst m (p,t)
    setst (p',TL h t')

data Dev = Dev Int
 deriving (Eq, Ord, Show)

instance PropRep Dev where
    to_prop (Dev i) = [("dev",PropInt i)]
    merge_prop ps (Dev i) = maybe (Dev i) Dev (prop_int $ lookup "dev" ps)

class DevelC a where
    devel :: a -> Maybe Int -> (a,Int)

instance DevelC t => DevelC (TL h t)
    where devel = tl_lift devel

instance DevelC (TL Dev t)
    where devel = tl_upd

instance Val Dev Int where
    val (Dev d) = d
    setv _ d = Dev d

{-
tick_devel :: DevelC s => StateMonad (p,s) ()
tick_devel = modlst (\s -> fst $ devel s (Just $ succ $ snd $ devel s Nothing))
-}

tick_devel :: DevelC d => SM_ d
tick_devel = atom $ modlst (\s -> fst $ devel s (Just (succ (snd (devel s Nothing)))))

at n m = when_devel (==n) m

when_devel p m = do
    dv <- get_lst_f (snd . (`devel` Nothing))
    when (p dv) m

with_devel f = do
    dv <- get_lst_f (snd . (`devel` Nothing))
    f (fromIntegral dv)


every n m = when_devel (\a -> a `mod` n == 0) m


instance NodeState t => NodeState (TL Dev t) where
    node_init = TL (Dev 0) node_init
--    state_hash (TL d t) = liftM2 (++) (state_hash d) (state_hash t)

instance PropRep Nil where
    to_prop _ = []
    merge_prop _ s = s

instance NodeState Nil where
    node_init = Nil
--    state_hash _ = Just []

data Pw = Pw Power
 deriving (Eq, Ord, Show)

prop_int (Just (PropInt i)) = Just i
prop_int _ = Nothing

prop_float (Just (PropFloat f)) = Just f
prop_float _ = Nothing

instance PropRep Pw where
    to_prop (Pw p) = [("pw", PropFloat p)]
    merge_prop ps (Pw p) = maybe (Pw p) Pw (prop_float $ lookup "pw" ps)

class PwC a where
    pw :: a -> Maybe Power -> (a,Power)

instance Val Pw Power where
    val (Pw p) = p
    setv _ p = Pw p

instance NodeState Pw where
    node_init = Pw 0
--    state_hash (Pw p) = Just [round p]

get_pw :: PwC s => SM s Power
get_pw = atom $ get_lst_f (snd . (`pw` Nothing))

use_pw :: (Val s Power, PwC (TL s t)) => Power -> StateMonad (RunState (TL s t)) ()
use_pw p' = map_val (\p->p-p')
gen_pw p' = use_pw (-p')

instance (NodeState t, DevelC t) => NodeState (TL Pw t) where
    node_init = TL (Pw 0) node_init
--    state_hash (TL p t) = liftM2 (++) (state_hash p) (state_hash t)

{-
tick_power :: (Val a Power, PwC (TL a b), DevelC (TL a b)) => SM_ (TL a b)
tick_power = do
        p <- get_pw
        when (p>=1) $ do
            use_pw 1
            tick_devel
-}

instance PwC t => PwC (TL h t) where
    pw = tl_lift pw

instance PwC (TL Pw t) where
    pw t m = tl_upd t m



send_down msg = push_cmd (Message Down msg)
send_up   msg = push_cmd (Message Up msg)



nop :: SM_ s
nop = return ()



on_msg msg ac = do
    ms <- getstp (pst_msg.ps_tmp.fst)
    mapM_ act ms
 where act m = when (msg == m) ac

type PW = SM_ (TL Pw (TL Dev Nil))

{-
plant_pw_palm_leaf = palm_leaf_devel (st::PW) (lf::PW)
 where st = do tick_power
               at 0 $ gen_pw 40
               p <- get_pw
               if p /= 1 then nxt st
                 else do
                  send_down "stop"
                  loop $ do
                   tick_devel
                   when_devel (>50) $ do
                     send_down "die"
                     set_pos 0.03
                     stop
       lf = do tick_power
               at 0 $ gen_pw 60
               on_msg "stop" (replace_prog (on_msg "die" (push_cmd Die)))


plant_prim_palm_leaf = palm_leaf_devel (tick_devel::SM_ (TL Dev Nil)) (tick_devel::SM_ (TL Dev Nil))

palm_leaf_devel sm lm = palm_leaf_skel
    (sm >> with_devel (set_pos . (/100) . fromIntegral))
    (do lm
        with_devel (set_pos . (/60) . fromIntegral))


--palm_leaf_skel :: (Show s, Show s', DevelC s, LocalState s, LocalState s') => SM s -> SM s' -> (String, Code)
palm_leaf_skel sm lm = spec "ppleaf_stem" leaf_stem
 where
    leaf_stem = do
        set_geom (mk_bendy_branch (Flat (0,1,0,1)))
        set_shape (\k -> (0.02*(1-k), (0,2*k,0)))
        sm
        when_devel (>5) $ do
            every 2 $ mapM_ (\i -> spawn_code (leaf_ori i) (spec "ppleaf" leaf)) [0,1]
    leaf_ori i _ =  rot_axis (0,1,0) (i*pi) `mulq` rot_axis (1,0,0) (pi/6)
    leaf = do
        set_geom (mk_branch (Flat (1,0,0,1)))
        set_shape (\k -> (0.01, (0,0.25*sin (k*pi/2),0) `rot_vec_q` rot_axis (1,0,0) (k/2)))
        lm
-}

{-
grav_bendy :: (String,Code) -> SM_ (TL Dev Nil)
grav_bendy sub = do
    set_shape (\_->(0,(0,0,0)))
    with_devel (\d -> 
        spawn_code (\_->rot_axis (1,0,0) (pi/16*d)) sub >>
        stop)
-}

rotx = rot_axis (1,0,0) . (2*pi*)
roty = rot_axis (0,1,0) . (2*pi*)
rotz = rot_axis (0,0,1) . (2*pi*)

{-
plant_pal = spec "palm" (m::PW)
 where
    m = do
        set_geom (mk_bendy_branch (Flat (0,1,0,1)))
        set_shape (\k -> (0.01, (0,k,0)))
        tick_devel
        let rest = do
                gen_pw 1
                pw <- get_pw
                when (pw > 60) (use_pw 60 >> get_pos >>= set_pos.(+0.03) >> replace_prog grow)
            grow = do
                tick_devel
                with_devel $ \d -> do
                 (`mapM_` [0..6]) $ \i ->
                  spawn_code (\_-> roty (d/16+i/6) `mulq` rotz (1/8)) (spec "palm_leaf" (grav_bendy plant_pw_palm_leaf))
                replace_prog rest
        grow
-}
        

        
{-
 plant_palm = spec "leaf" leaf_stem
  where
    leaf_stem = do
        set_geom (mk_bendy_branch (Flat (0,1,0,1)))
        set_shape (\k -> (0.1, (0,k,0)))
        set_growth 0.1
        gen_power 50
        let slow = do
            tick_devel
            grow_with 0.5
            with_power (\p -> when (p < 1) (replace_prog stop_prog))
        let sim = do
            tick_devel
            grow_with 1
            let lf i p = spawn_code (leaf_ori i) (spec "leaf" (leaf p))
            with_power (\pw -> let p = min 30 pw in every 5 (lf (-1) p >> lf 1 p))
            with_power (\p -> when (p < 15) (replace_prog slow))
        replace_prog sim
    leaf_ori i _ = rot_axis (1,0,0) (i*pi/3)
    leaf n = do
        tick_devel
        set_geom (mk_branch (Flat (1,0,0,1)))
        set_shape (\k -> (0.05, (0,k,0)))
        gen_power 1
        let sim = do
            tick_devel
            with_power $ \p -> 
                set_growth ((min 20 p) * 0.005)
            grow_with 1
        sim
        at 4 $ gen_power n >> replace_prog sim
-}

plant_palm_leaf :: SM_ Vars
plant_palm_leaf = do
    set_geom (mk_bendy_branch (Flat (0,1,0,1)))
    set_shape (\_ _ k -> (0.1, (0,k,0)))
    set_pos 2
    atom $ decl "foo" (1.0::Float)
    f <- atom $ get "foo"
    return (if f == (1::Float) then () else ())
    return ()



br_ori (k:l:_) =
    rot_axis (0,1,0) (2.23*k*pi) `mulq` rot_axis (0,0,-1) (l*pi/4)

bend _ = rot_axis (0,0,-1) (pi/6)


mk_plants = sequence (
--        liftM ((,) "pw_palm_leaf" ) (mk_sim_engine (seed plant_pw_palm_leaf)) : 
--        liftM ((,) "prim_palm_leaf" ) (mk_sim_engine (seed plant_prim_palm_leaf)) : 
--        liftM ((,) "pal" ) (mk_sim_engine (seed plant_pal)) : 
        liftM ((,) "palm_leaf" ) (mk_sim_engine (seed plant_palm_leaf)) : 
        liftM ((,) "palm_leaf" ) (mk_sim_engine (seed plant_palm_leaf)) : 
    [])


