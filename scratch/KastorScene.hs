
module KastorScene
 ( module KastorSceneBase,
   module KastorSceneAttr,
   module KastorSceneNode,
   module FixedPoint,
   module Action,
   module KTypes,
   Scene,
   href,
   Bind,
   bind
 ) where

import KastorSceneBase
import KastorSceneTemplates
import KastorSceneAttr
import KastorSceneNode
import KTypes
import FixedPoint
import Action

type Scene = NodeT SVG -> NodeT SVG

href h = xlink_COLON_href (MkAttrString h)

class Bind v where
  bind :: BindingVarName -> (v -> NodeT n -> NodeT n) -> NodeT n -> NodeT n

instance Bind Int16x16 where
  bind var f (NodeT (Node n as cs bs)) = NodeT (Node n as cs (b:bs))
   where b = Binding Nothing var AttrType_16x16 v
         v = case f undefined (NodeT (Node undefined [] [] [])) of
              NodeT (Node _ ((a,_):_) _ _) -> a

type BindingVarName = String

