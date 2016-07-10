
module KastorLab where

import Graphics.UI.WX as WX
import Graphics.UI.WXCore
import qualified Graphics.UI.WXCore as WXC

import KTypes
import KInterface
import KastorScene

import Signals
import Data.IORef

import KastorView
import KastorInstance
import EvalScript
import ScriptEditView

import Control.Monad.Identity

import DeepSeq
import Control.Exception
import FixedPoint
import IDGen

import Misc

import KastorSceneNodeOp
import KastorSceneManager

import SceneView
import TriggerView
import WatcherView
import UpdateLogView

kl_main = start $ do
  let w = 240; h = 320
  bus <- initBus
  k <- createKastor w h 

  
  
  f <- frame [ text := "KastorLab" ]

  left <- panel f []
  right <- panel f []

  right_split <- splitterWindow right []
  right_upper <- panel right_split []
  right_upper_split <- splitterWindow right_upper []


  kv@(KastorView _ kwnd _) <- createKastorView k left w h []
  wv <- watcherView bus left
  tv <- triggerView bus left


  lv <- logView bus right_split



  code <- scriptEditView bus right_upper_split
  svg <- sceneViewGraph bus right_upper_split


  withKastor k $ \core -> do
    kcore_registerResourceDirectory core "data/"

  -- layout
  WX.set right_upper [ layout := WX.fill $
        vsplit right_upper_split 5 600
        (WX.fill $ widget code) (WX.fill $ widget svg) ]

  WX.set right [ layout :=
      WX.fill $ hsplit right_split 5 600
        (WX.fill (widget right_upper))
        (WX.fill (widget lv)) ]
 
  WX.set left [ layout := 
       column 5 [widget kwnd, WX.fill (widget wv), WX.fill (widget tv)] ] 

  WX.set f [ layout := WX.fill $ 
    row 5 [WX.vfill (widget left), WX.fill (widget right) ] ]

  WXC.splitterWindowSetSashPosition right_split 300 True
  WXC.splitterWindowSetSashPosition right_upper_split 500 True

  WX.set f [ size := sz 1200 700 ]

  monitor bus "kastor-scene-dirty" (\() -> repaint kwnd)

  WX.set f [ on closing :~ \c -> do
    signal bus "kastor-scene-update" [RemoveNode Root :: NodeOp]
    destroyKastor k
    c ]


-- setup update
  ks <- newScene
  monitor bus "kastor-scene-update" $ \ns -> do
   withKastor k $ \core -> do
    applyChanges core ks ns
    kcore_getRoot core >>= knode_playAnim
    signal bus "kastor-scene-dirty" ()

-- scene (re)loading
  monitor bus "reload-code" $ \(fn,cts) -> do
    let _ = fn :: String; _ = cts :: String
    is_ref <- mkStream
    (scene,unload) <- eval cts "scene"
    print $ scene (NodeT (Node "" [] [] []))
    unload
    (actions,unload) <- eval cts "actions"
    let _ = scene :: Scene; _ = actions :: Actions SVG
    (ns,bs) <- runId is_ref $ do
      KastorSceneManager.insertTree Root (root scene)
    mapM_ (signal bus "kastor-scene-binding") bs
    signal bus "kastor-scene-update" (RemoveNode Root : ns)

    foreachM_ actions (\(event,[(target,action)]) -> do
      monitor bus ("trigger-"++event) $ \() -> do
        (as,bs) <- runId is_ref $ do
          KastorSceneManager.insertTree (target) (root action)
        signal bus "kastor-scene-update" as
        mapM_ (signal bus "kastor-scene-binding") bs)


-- load scene code
  let fn = "Scene1.hs"
  cts <- readFile fn
  let edit = cts `deepSeq` (fn,cts)
  signal bus "edit-code" edit
  signal bus "reload-code" edit
--  signal bus "reload-code" edit

  print "Setup done"






