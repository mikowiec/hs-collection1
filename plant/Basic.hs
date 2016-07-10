module NonBasic where

-- Distribution Probability Function [0..1] -> [0..1]
-- used for placing branches on a trunk etc
type DistFunc = (Float -> Float)

-- Positional Function [0..1] -> ([0..1], [0..1], [0..1])
-- used for forming the shape of branches
type PosFunc = (Float -> (Float, Float, Float))

type Branchdist = DistFunc
type Angledist = DistFunc
type Spawnrate = Float

data Interface = IF Branchdist Spawnrate Angledist

type Point = (Float, Float, Float)
type Polygon = [Point]
type Spline = [Point]

type ShapeOne = Spline
type ShapeTwo = Polygon
type ShapeComplex = [Polygon]

type GrowthRate = Float
data Earth = Ea GrowthRate

data PlantNode = Br PosFunc
               | Le ShapeTwo
               | Fl ShapeComplex

data Plant = Pl Earth [Crown]

data Crown = Cr Interface PlantNode [Crown]

-- Example components
stdbranchif::Interface
stdbranchif = (IF id 0.0 id)
stdflowerif = (IF id 0.0 id)

stdflower::PlantNode
stdflower = Fl [[]]

stdbranch::PlantNode
stdbranch = Br (\param -> (0.0, param, 0.0))

stdleaf::PlantNode
stdleaf = Le []

-- Example 1: Flower
flower::Plant
flower = Pl (Ea 1.0) [(Cr stdbranchif stdbranch [(Cr stdflowerif stdflower [])])]

-- Example 2: Tree
tree::Plant
tree = Pl (Ea 1.0) [(Cr stdbranchif stdbranch 
                    [(Cr stdbranchif stdbranch 
                        [(Cr stdbranchif stdbranch 
                            [(Cr stdbranchif stdbranch 
                                [(Cr stdbranchif stdleaf
                                [])] ) ] ) ] ) ] ) ]
                                
                                