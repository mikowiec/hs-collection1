
module WatcherView where

import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXC
import Signals
import Data.IORef
import KastorSceneBase
import KastorSceneNodeOp
import KTypes
import Misc
import Maybe
import Control.Exception
import FixedPoint

try_ m = try m >> return ()

watcherView bus f = do
  p <- panel f []
  scr <- scrolledWindow p [ scrollRate := sz 0 10 ]

  bs_ref <- newIORef []
  up_ref <- newIORef []

  let set_value txt nl attr = try_ $ do
        val <- get txt text
        r <- readIO val :: IO Float
        let i = let (u,l) = divMod (round (r*0x10000)) 0x10000
                in Int16x16 (fromIntegral u) (fromIntegral l)
        signal bus "kastor-scene-update" [SetAttr nl attr (AttrX i)]
  let relayout = do
       cs <- get scr children
       foreachM cs $ \c -> windowRemoveChild scr c
       bs <- readIORef bs_ref
       bvs <- (`mapM` bs) $ \(ModifyAttr binding nl attr) -> do
        p' <- panel scr []
        t <- staticText p' [ text := binding ++ " : " ++ attr ]
        v <- textEntry p' [ text := "" ]
        b <- button p' [ text := "Set", on command := set_value v nl attr ]
        set v [ on enterKey := set_value v nl attr]
        set p' [ layout := WX.fill $ row 5 [widget t, WX.fill $ widget v, widget b] ]
        let show' (AttrX (Int16x16 i f)) = show ((fromIntegral i + fromIntegral f / 0x10000) :: Float)
        modifyIORef up_ref (((nl,attr), (\val -> set v [ text := show' val ] )) : )
        return p'
       set scr [ layout := 
         ((if null bvs then glue else (column 5 (map widget bvs))) ) ]
       
       return ()

  monitor bus "kastor-scene-update" $ \as -> do
    let up (SetAttr nl attr val) = do
         us <- readIORef up_ref
         withM (lookup (nl,attr) us) ($ val)
        up _ = return ()
    mapM_ up as

  monitor bus "kastor-scene-binding" $ \(b::Bind) -> do
    modifyIORef bs_ref (b:)
    relayout

  set p [ layout := fill ((widget scr)) ]

  return p


