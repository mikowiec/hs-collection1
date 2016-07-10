
module SceneView where

import Graphics.UI.WX as WX hiding (when)
import Signals
import KastorSceneBase
import Data.IORef
import qualified Data.Map as M
import ApplyNodeOps
import IDGen
import Monad
import Misc

sceneViewText bus f = do
  p <- panel f []
  txt <- textCtrl p [ font := fontFixed ]

  monitor bus "kastor-scene" $ \root -> do
    set txt [ text := show (root :: Node) ]
  set p [ layout := (fill $ minsize (sz 300 200) $ widget txt) ]
  return p

node_width :: Num a => a
node_width = 50

node_height = 30
node_height_2 = node_height `div` 2

node_width_2 = node_width `div` 2

drawScene ref dc viewArea = do
  WX.set dc [fontFace := "Courier New", fontSize := 6 ]
  tree <- readIORef ref
  case tree of
   Nothing -> return ()
   Just tree@(TreeNode id name off dv xs) -> do
    let mid_x = ((round $ (dv*node_width) + node_width*(-off)))
    drawTree dc mid_x 35 mid_x 40 tree
    return ()

drawTree dc x' y' x y (TreeNode id name off dv xs) = do
  line dc (pt x' (y'+node_height_2)) (pt x (y-node_height_2)) [WX.color := red, penKind := PenSolid]
  let id_s = show id
      id_size_2 = (6 * length id_s) `div` 2
      text_size_2 = (6 * length name) `div` 2
  drawRect dc (Rect (x - node_width_2) (y - node_height_2) node_width node_height) [WX.color := blue, penKind := PenSolid]
  drawText dc id_s (pt (x-id_size_2) (y-10)) []
  drawText dc name (pt (x-text_size_2) y) []
  let is = [ round (node_width*(off + i*dv)) | i <- [0..] ]
  rs <- mapM (\(i,t) -> drawTree dc x y (x+i) (y+50) t) (zip is xs)
  let (ws,hs) = unzip rs
  return (maximum (x : ws), maximum (y : hs))
  


data RefTree = RefTreeNode ID String (Maybe RefTree) (IORef [RefTree])

data FrozenTree = FrozenNode ID String [FrozenTree]
 deriving Show

data SpacedTree = TreeNode ID String Float Float [SpacedTree]
 deriving Show

freezeTree (RefTreeNode id name _ r) = do
  xs <- readIORef r
  fs <- mapM freezeTree xs
  return (FrozenNode id name fs)


spaceTree (FrozenNode id name xs) = 
  let (ws,xs') = unzip (map spaceTree xs)
      w = max 1 (sum ws)
      n = fromIntegral $ max 0 (pred (length xs'))
  in (w, TreeNode id name (- (w / 2)) (w / n) xs')


vt_ops :: ApplyOps (IORef (Maybe RefTree)) RefTree
vt_ops = ApplyOps
 (\t ns -> return t)
 (\t p name ->
   do id <- mkId
      r <- newIORef []
      let n = RefTreeNode id name p r
      case p of
       Nothing -> writeIORef t (Just n)
       Just r -> do RefTreeNode _ _ _ ref <- return r
                    modifyIORef ref (++[n])
      return (n,t))
 (\t n -> case n of
           RefTreeNode id _ (Just (RefTreeNode _ _ _ rs)) _ -> do 
                modifyIORef rs (dropNode id)
                return t
           _ -> return t)
 (\t n l a ->
   do return t)

dropNode id (RefTreeNode id' _ _ _ : xs) | id == id' = xs
dropNode id (x:xs) = x : dropNode id xs
dropNode _ [] = []

sceneViewGraph bus f = do
  p <- panel f []
  scr <- scrolledWindow p [ scrollRate := sz 10 10 ]
  ref <- newIORef Nothing
  canvas <- window scr [ bgcolor := white, on paint := drawScene ref]

  set scr [ layout := fill (widget canvas) ]
  set p [ layout := fill $ widget scr ]

  r <- newIORef Nothing
  db_ref <- mkApplyDb
  monitor bus "kastor-scene-update" $ \ns -> do
   mapM_ (applyOp vt_ops r db_ref) ns
   tree <- readIORef r
   tree' <- fmapIO (\x -> spaceTree $^ freezeTree x) tree
   writeIORef ref (fmap snd tree')
   repaint scr
  return p

fmapIO :: (a -> IO b) -> Maybe a -> IO (Maybe b)
fmapIO f Nothing = return Nothing
fmapIO f (Just v) = do r <- f v
                       return (Just r)


sv_main = do
  f <- frame [ text := "SceneView" ]
  w <- window f []
  set f [ layout := fill (widget w) ]
  let cs = [FrozenNode 1 "left" [], FrozenNode 2 "right" []]
  let tree = FrozenNode 0 "root" cs
  let (_,tree') = spaceTree tree
  print $ tree'
  ref <- newIORef (Just tree')
  set w [ bgcolor := white, on paint := drawScene ref]


