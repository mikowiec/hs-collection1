module NonBasic where

import Types
import PlantTypes
import LinAlg
import MonadLib
import GeomRep hiding (Branch)
import Array
import Data.Array.MArray
import Data.Array.ST
import ST

-- These are used to do intersection tests for leaves... oh yes, they are spheres!
type Sphere = (Point, Radius)
type SunWorld = [Sphere]
type Ray = (Point, Vector)


-- Distribution Probability Function [0..1] -> leaf_count -> [0..1]
-- used for placing branches on a trunk etc
--                           RelativePosition -> NrOfBranches -> Position -> Development -> Mothers Development -> Distribution Probability
data DistFunc = Df {
                    df_shape :: (DistPart -> Int -> Development -> Development -> Development -> Float),
                    df_strict:: Float
                   }
                   
type PointFunc = Point

-- Positional Function [0..1] -> ([0..1], [0..1], [0..1])
-- used for forming the shape of branches
type PosFunc = (BranchState -> Float -> Point)

type BranchDist = DistFunc
type Angledist = (Float -> Vector -> Point) -- Random -> ForwardVector -> Direction
type Spawncnt = Int
type Evolvefnc = (Float -> Development -> Development -> Bool)
type EvFnc = (Float -> Development -> Development)

--               bs             md             in dev         out dev
type ModDevFnc = BranchState -> Development -> Development -> Development
-- new evolve functions


type ModelPos = (Point, Quat)

data Flags = FeedLeaves | EmptyFlag deriving (Eq, Ord, Ix)
type FlagReg = Array Flags Bool

type FloatStack = [Float]

data Vitals = Vi {
        vi_cbs:: [Int],
        vi_md :: Development
     }


data State s = St { 
                 st_rnd :: Rnd,
                 st_seed :: Seed, 
                 st_env :: Environment, 
                 st_pos :: ModelPos,
                 st_md :: Development,
                 st_powout :: Power,
                 st_powback :: Either Power Power,
                 st_time :: Time,
                 st_err :: String,
                 st_idcnt :: Int,
                 st_powacc :: Power,
                 st_flags :: FlagReg,
                 st_fallen :: [Int],
                 st_fstack :: FloatStack,
                 st_barr :: BArr s,
                 st_cbs :: [Int],
                 st_vit :: [Vitals],
                 st_grpr :: Float
             }
                           
             
data BArr s = BArr { 
                     barr_arr :: STArray s Int BranchState,
                     barr_max :: Int,
                     barr_curr :: Int
                   }

data IState = ISt { 
                 ist_rnd :: Rnd,
                 ist_seed :: Seed, 
                 ist_env :: Environment, 
                 ist_idcnt :: Int,
                 ist_powacc :: Power,
                 ist_flags :: FlagReg,
                 ist_fallen :: [Int],
                 ist_fstack :: FloatStack,
                 ist_barr :: IBArr,
                 ist_cbs :: [Int],
                 ist_grpr :: Float
             }
                           
 

data IBArr = IBArr { 
                  ibarr_arr :: Array Int BranchState,
                  ibarr_max :: Int,
                  ibarr_curr :: Int
                   }



-- type SimStateP r s = StateMonadP (BArr s) (ST s) r

type SimState s r = StateMonadP (State s) (ST s) r

type SimStateI r = StateMonad IState r 
-- type SimState a = StateMonad State a

init_last::Maybe Development
init_last = Nothing

zero_leaves = 0;

null_state::BranchState
null_state = BS [] origo origo 0.0 0.0 0.0 0.0 Branch [] [] Dead True (-1) [([0],0.0)] init_last undefined 0

birth_phys::PhysStat
birth_phys = Alive 1.0

null_env::Environment
null_env = En 1.0 []

type Simlist = [(Resrc, Time)]
data Tree = Tree {
    tree_simlist :: Simlist,
    tree_rep     :: Maybe (RenderData),
    tree_state   :: IState
  }


type TreeState s a = StateMonadP Tree (ST s) a


data Interface = IF {
                    if_bdist :: BranchDist,
                    if_scnt  :: Spawncnt,
                    if_adist :: Angledist,
                    if_sdev  :: Development, -- development before level starts
                    if_evf   :: EvFnc,
                    if_moddev:: ModDevFnc
                 }

instance Show Interface where
    show (IF _ _ _ _ _ _) = "Interface"

type Gravity = Float

data Environment = En {
                    en_grav :: Gravity,
                    en_sunw :: SunWorld
                   } deriving Show


-- type Point = (Float, Float, Float)
type Polygon = [Point]
type Spline = [Point]

type ShapeOne = Spline
type ShapeTwo = Polygon
type ShapeComplex = [Polygon]

type GrowthRate = Float
type BreedRate  = Int
type Resrc = [(String, Float)]
type RootFunc = Float -> Resrc -> Float
data Earth = Ea {
               ea_grate :: GrowthRate, 
               ea_brate :: BreedRate, 
               ea_rootf :: RootFunc,
               ea_boost :: Float
             }

instance Show Earth where
    show (Ea gr br rf bo) = (show gr) ++ " / " ++ (show br) ++ " / " ++ (show bo)          

type LeafCons = Float
type SurFunc = Development -> Development -> Bool
type Texture = String
data PlantNode = Br {
                  br_posfnc :: PosFunc,
                  br_emod   :: EMod,
                  br_color  :: Color,
                  br_bump   :: Point,
                  br_thick  :: Float
                 }
               | Le {
                  le_shaptwo :: ShapeTwo, 
                  le_cons    :: LeafCons,
                  le_surfnc  :: SurFunc,
                  le_texalive:: Texture,
                  le_texfaded:: Texture,
                  le_size    :: Float
                 } 
               | Fl ShapeComplex

instance Show PlantNode where
 show (Br _ _ _ _ _) = "branch"
 show (Le _ _ _ _ _ _) = "leaf"
 show (Fl _) = "flower power!"

data Common = Co {
        } deriving Show

data Seed = Se {
              se_earth :: Earth,
              se_comin :: Common,
              se_crown :: [Crown]
            }
    deriving Show

data Crown = Cr Interface PlantNode [Crown]
    deriving Show
