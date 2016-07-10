
module KastorSceneNode where

import KastorSceneBase
import KastorSceneAttr
import KastorSceneTemplates
import NodeDB
import Data.Map
import Monad

$( defineNode "SVG" )
$( defineInst "SVG" ["x", "y", "width", "height" ] )

$( defineNode "Image" )
$( defineInst "Image" ["x", "y", "width", "height", "xlink_COLON_href"] )

$( defineNode "Rect" )
$( defineInst "Rect" ["x", "y", "width", "height", "cfill"] )

-- FIXME
$( defineNode "AnimateTransform" )
$( defineInst "AnimateTransform" ["attributeName", "begin", "dur", "end", "from", "to", "fill"] )

$( defineNode "Animate" )
$( defineInst "Animate" ["attributeName", "begin", "dur", "end", "from", "to", "fill"] )

$( defineNode "PixelEffect" )
$( defineInst "PixelEffect" ["x","y","width","height","effect_type","opacity"] )

$( defineNode "KernelEffect" )
$( defineInst "KernelEffect" ["x","y","width","height","effect_type","opacity"] )

$( defineNode "Particles" )

$( defineNode "Emitter" )
$( defineInst "Emitter"
   ["shape", "x0", "x1", "y0", "y1",
    "intensity", "xlink_COLON_href",
    "particlewidth", "particleheight",
    "angle", "anglespread",
    "velocity", "velocityspread",
    "lifespan", "lifespanspread",
    "fadeindur", "fadeoutdur"] )

$( defineNode "Affecter" )

nodeDefs :: IO (Map String [String])
nodeDefs = nodeMap

$( concat `liftM` mapM (`defineInst` ["node_id"]) ["SVG", "Image", "Rect", "AnimateTransform", "Animate"] )


