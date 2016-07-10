
module UpdateLogView where

import Graphics.UI.WX as WX
import Signals
import KastorSceneNodeOp
import Data.IORef
import IDGen

logView bus p = do
  t <- textCtrl p [ font := fontFixed ]
  monitor bus "kastor-scene-update" $ \ns -> do
    let _ = ns :: [NodeOp]
    org <- get t text
    set t [ text := org ++ concatMap ((++"\n") . show) ns ]
  return t



