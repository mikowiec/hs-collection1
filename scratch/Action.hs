
module Action where

import KastorSceneNodeOp
import KastorSceneBase

type Event = String
type Target = NodeLocator

type ActionList n = [(Target, NodeT n -> NodeT n)]

type Actions n = [(Event, ActionList n)]



