
module GeomRep where

import Types
import Array
import LinAlg
import Pf

type ID = Int
type Width = Float
type Bump = Point

newtype Fn a = Fn a

instance Eq (Fn a) where (==) _ _ = False

instance Show (Fn a) where
    show _ = "<fn>"

instance Read (Fn a) where
    readsPrec i s = []

-- Geometry & Visuals
data GeomPrim
    = Li Age Age Point Point
    | LiA Width Width Point Point Color Bump
    | Pt Point Vector
    | PtA Point Vector Size Surface 
    | Branch Surface Width Width Point Point
    | FnBranch Surface [Float] (Fn (Float -> (Width,Point)))
    | Square Surface Size
    | Surface Surface Vector Vector Vector Vector {- left right mid top -}
 deriving (Read,Show)

type GeomRep = [GeomPrim] 


type Orient = Quat
type Scale = Vector
type TagID = Int
type Param = (Float,Vector)

data GNode
    = Offset Vector GNode
    | Scale Scale GNode
    | Ori Orient GNode
    | Tag TagID GNode
    | Group [GNode]
    | Geometry GeomRep
    | Param (Fn (Param -> GNode))
 deriving (Show,Read)

data LocGeom = LocGeom Vector Orient Scale Int GeomRep
 deriving Read

instance Show LocGeom where
    show (LocGeom v o s i gr) =
        sprintf ("LocGeom "&" "&" "&" "&" "&"") v o s i (apply_lod gr)


flatten :: GNode -> [LocGeom]
flatten = flatten' [] (0,0,0) id_quat (1,1,1) (-1)
flatten' acc lp ori sc i gn = case gn of
    Geometry gr -> let gr' = LocGeom lp ori sc i gr
                   in  (gr':acc)
    Scale sc' gn -> flatten' acc lp ori (sc `cmul` sc') i gn
    Ori ori' gn -> flatten' acc lp (ori `mulq` ori') sc i gn
    Offset v gn -> flatten' acc (lp `add` (v `rot_vec_q` ori)) ori sc i gn
    Tag i gn -> flatten' acc lp ori sc i gn
    Group gn -> concatMap (flatten' acc lp ori sc i) gn
--    Param (Fn f) -> flatten' acc lp ori sc (f (0,(0,-1,0)))

unflatten :: [LocGeom] -> GNode
unflatten = Group . map f
 where f (LocGeom v o s t gr) = Tag t (Offset v (Ori o (Scale s (Geometry gr))))
  -- is this correct?
 
apply_param p@(pm,g) gn = case gn of
    Param (Fn f) -> apply_param p (f p)
    Offset v gn  -> Offset v (apply_param p gn)
    Scale s gn   -> Scale s (apply_param p gn)
    Ori o gn     -> Ori o (apply_param (pm,g `rot_vec_q` (inv o)) gn)
    Tag i gn     -> Tag i (apply_param p gn)
    Group gs     -> Group (map (apply_param p) gs)
    Geometry gs  -> Geometry (apply_lod gs)

apply_lod gs = concatMap f gs
 where f (FnBranch s ps (Fn f)) = 
           let xs = map f ps
               ws = zip xs (tail xs)
           in [ Branch s w1 w2 p1 p2 | ((w1,p1),(w2,p2)) <- ws ]
       f g = [g]


-- All traversals of GNode must correspond to (flatten . apply_param p)


