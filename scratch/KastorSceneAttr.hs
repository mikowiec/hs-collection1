
module KastorSceneAttr where

import KastorSceneBase
import KastorSceneTemplates
import FixedPoint
import KTypes

type Int16x16List = [Int16x16]

$( defineAttrs ''Int16x16 ["x", "y", "width", "height" ] )
$( defineAttrs ''Int16x16List ["from", "to"] )

$( defineAttrs ''AttrTimeMS ["begin", "end", "dur"] )

$( defineAttr ''AttrColor "cfill" "fill" )
$( defineAttrs ''KID ["fill"] )
$( defineAttrs ''KID ["additive", "type"] )
$( defineAttrs ''AttrString ["xlink_COLON_href"] )
$( defineAttrs ''KID ["attributeName"] )

$( defineAttr ''AttrString "node_id" "id" )

$( defineAttrs ''Int16x16 ["opacity"] )

$( defineAttr ''KID "effect_type" "type" )

$( defineAttrs ''KID ["shape"] )

$( defineAttrs ''Int16x16
   ["x0", "x1", "x2", "x3", "y0", "y1", "y2", "y3",
    "intensity", 
    "particlewidth", "particleheight",
    "angle", "anglespread",
    "velocity", "velocityspread",
    "lifespan", "lifespanspread",
    "fadeindur", "fadeoutdur"] )

