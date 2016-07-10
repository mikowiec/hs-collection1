
module ApplyNodeOps where

import KTypes
import KastorSceneNodeOp
import Data.IORef
import Data.Map as M
import IDGen
import Monad
import Maybe

data ApplyOps t n = ApplyOps {
   onRestart :: t -> [n] -> IO t,
   onCreate :: t -> Maybe n -> String -> IO (n,t),
   onRemove :: t -> n -> IO t,
   onSetAttr :: t -> n -> String -> Attribute -> IO t
 }

type NodeDB n = (M.Map ID n, M.Map String ID)

type NodeId = ID

mkApplyDb = 
  newIORef (M.empty, M.empty)

applyOp :: ApplyOps t n -> t -> IORef (NodeDB n) -> NodeOp -> IO t


applyOp ops t ref (CreateNode mpid cid name) = do
  (idb,ndb) <- readIORef ref
  t' <- case mpid of
         Root | M.null idb -> applyOp ops t ref (RemoveNode Root)
         _ -> return t
  parent <- case mpid of
             NodeWithName n -> case M.lookup n ndb of
                                Just id -> Just `liftM` lookupNode ref id
                                Nothing -> return Nothing
             NodeWithId pid -> Just `liftM` lookupNode ref pid
             Root -> return Nothing
  (n,t') <- onCreate ops t parent name
  modifyIORef ref (\(db,ndb) -> (M.insert cid n db, ndb))
  return t'

applyOp ops t ref (RemoveNode Root) = do
  (db,_) <- readIORef ref
  t' <- onRestart ops t (M.elems db)
  writeIORef ref (M.empty,M.empty)
  return t'
  
applyOp ops t ref (RemoveNode (NodeWithId id)) = do
  node <- lookupNode ref id
  t' <- onRemove ops t node
  modifyIORef ref (\(idb,ndb) -> (M.delete id idb, M.filter (/=id) ndb))
  return t'

applyOp ops t ref (SetAttr (NodeWithId id) kid val) = do
  node <- lookupNode ref id
  t' <- onSetAttr ops t node kid val
  when (kid == "id") $ case val of AttrString name -> modifyIORef ref (\(idb,ndb) -> (idb, M.insert name id ndb))
  return t'


lookupNode ref id = do
  (db,_) <- readIORef ref
  Just node <- return $ M.lookup id db
  return node
 `catch` (\e -> readIORef ref >>= \db -> fail ("Cannot find " ++ show id ++ "\n" ++ show e))


