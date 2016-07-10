
module KastorSceneNodeOp where

import KTypes
import Data.Typeable


data NodeLocator = Root | NodeWithId Int | NodeWithName String
 deriving (Show, Typeable, Eq, Ord)

data NodeOp
 = CreateNode NodeLocator Int String
 | RemoveNode NodeLocator
 | SetAttr NodeLocator String Attribute
 deriving (Show, Typeable)

data Bind = ModifyAttr String NodeLocator String
 deriving (Show, Typeable)

