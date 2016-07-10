
module PlantTypes where

import Random
import Pf
import Misc
import Types
import GeomRep

type Position = Float -- send to posfunc for mother
type Orientation = Quat
type Development = Float 
type PlantId = [Int]
type Rnd = StdGen
type Power = Float
type EMod = Float
type Mass = Float
type Moment = Float

data DistPart = Children Float
              | Self
              | RootSelf    deriving Show
      
data LevMode = Branch 
             | Leaf         deriving Show

data BranchState = BS {
   bs_bs :: [Int],
   bs_dir :: Direction,
   bs_pnt :: Point,
   bs_pos :: Position,
   bs_rad :: Radius,
   bs_dev :: Development,
   bs_age :: Age,
   bs_mode :: LevMode,
   bs_pin :: PlantId, -- plant Id
   bs_min :: PlantId,  -- mother Id
   bs_alive:: PhysStat,
   bs_more:: Bool,
   bs_id:: Int,
   bs_nxtev:: [(PlantId,Development)],
   bs_last :: Maybe Development,
   bs_rnd :: Rnd,
   bs_lcnt :: Float
   
   }

data PhysStat = Alive {
         ps_mood :: Float
         }
              | Dead        deriving Show

data RenderData = RenDat {
        rd_geomrep :: GeomRep,
        rd_campos :: (Double, Double, Double),
        rd_camlookat :: (Double, Double, Double)
        } deriving Show



instance Show BranchState where
    showsPrec n (BS cs dir pnt pos rad dev age mode pin min alive more bsid nxd last rnd lcn) =
      let indent = replicate n ' ' in
      showString indent .
        let cld | null cs   = showString ">\n"
                | otherwise = showString "\n" . compMap (\c -> showsPrec (n+3) c) cs . showString (indent++">\n")
        in ssprintf ("<BS dir:"&>pt_fmt<&" pos:"&" rad:"&" dev:"&>flt 2<&" age:"&>flt 2<&" mode:"&" pin:"&" min:"&" alive:"&" id:"&" nxt:"&" last: "&"")
                        dir pos rad dev age mode pin min alive bsid nxd last. cld

