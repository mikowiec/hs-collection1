
module KastorSceneManager where

import IDGen
import qualified KTypes as K
import qualified KInterface as KI
import KastorSceneBase
import qualified Data.Map as M
import Data.IORef
import FixedPoint
import KastorSceneNodeOp
import ApplyNodeOps
import Monad
import Control.Monad.State
import Control.Monad.Trans
import Foreign.Ptr

data KastorScene = KastorScene (IORef (NodeDB KI.KNode))


emptyNode = NodeT (Node undefined [] [] [])

untypedNode (NodeT n) = n

newScene = do
  ref <- mkApplyDb
  return (KastorScene ref)

data EmitColl a = EmitColl [a]

emit x = modify (\(EmitColl xs) -> EmitColl (x:xs))

nil = EmitColl []

insertTree rootLoc node = do
  ((_,EmitColl xs),EmitColl ys) <- runStateT (runStateT (insertTree' rootLoc node) nil) nil
  return (reverse xs::[NodeOp],ys::[Bind])

insertTree' rootLoc (Node name attrs children bindings) = do
 let
  tie i (Binding Nothing n t attr_id) =
    ModifyAttr n (NodeWithId i) attr_id
 i <- lift $ lift $ genId
 emit $ CreateNode rootLoc i name
 mapM_ emit $ map (\(l,v) -> SetAttr (NodeWithId i) l v) attrs
 mapM_ (lift . emit . tie i) bindings
 mapM_ (insertTree' (NodeWithId i)) children

applyChanges :: KI.KCore -> KastorScene -> [NodeOp] -> IO ()
applyChanges kCore (KastorScene ref) ops = do
  mapM_ (apply kCore ref) ops

sm_ops kCore ref = ApplyOps
  (\_ ns -> mapM_ KI.knode_release ns >> KI.kcore_setRoot kCore (KI.KNode nullPtr))
  (\_ p name ->
    do n <- KI.kcore_createNode kCore name
       case p of
        Just p  -> KI.knode_appendChild p n
        Nothing -> KI.kcore_setRoot kCore n
       return (n,()))
  (\_ n ->
    do knode_drop n
       KI.knode_release n)
  (\_ n kid val ->
    do case reads ("KID_"++kid) of
        [(kid',_)] -> KI.knode_setDocAttr n kid' val)

apply kCore ref op = applyOp (sm_ops kCore ref) () ref op

knode_drop node = do
  p <- KI.knode_getParent node
  KI.knode_RemoveChild p node

