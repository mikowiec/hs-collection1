
module KastorCode where

import Text.Printf
import Graphics.UI.WX
import KastorView
import KastorInstance

import KInterface

import Data.IORef
import qualified Data.Map as Map
import Data.Dynamic
import List
import Control.Exception
import Prelude hiding (catch)

import Signals
import ScriptEditView

import System.Plugins
import System.Eval.Haskell


kc_main = start $ do
  let w = 240; h = 320
  f <- frame [ text := "KastorCode" ]
  bus <- initBus
  
  k <- createKastor w h

  withKastor k $ \core -> do
   root <- kcore_createNode core "svg"
   kcore_setRoot core root
   knode_release root
  kv@(KastorView _ wnd _) <- createKastorView k f w h []
  set f [ on closing :~ \c -> destroyKastor k >> c ]

  scr <- scriptEditView bus f

  readIORef bus >>= print

  let fn = "TestScene.hs"
  code <- readFile "TestScene.hs"
  signal bus "edit-code" (fn,code)

  monitor bus "reload-code" $ \(fn,code) -> do
  {-
    Just mkScene <- eval code []
    withKastor k $ \core -> do
     n <- mkScene core
     kcore_setRoot core n
     knode_release n
    -}
    writeFile fn code
    print "make"
    ms <- make fn []
    case ms of
     MakeFailure err -> print err
     MakeSuccess _ obj -> do
      print "load"
      status <- load obj [] [] "mkScene"
      case status of
       LoadFailure msg -> print msg
       LoadSuccess mod mkScene -> do
        withKastor k $ \core -> do
         n <- mkScene core :: IO KNode
         kcore_setRoot core n
         knode_release n
        repaint wnd
        unload mod

  set f [ layout := fill (row 5 [column 5 [widget wnd], fill $ widget scr]) ]



