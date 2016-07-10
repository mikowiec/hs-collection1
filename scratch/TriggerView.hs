
module TriggerView where

import Graphics.UI.WX as WX
import Signals

triggerView bus f = do
  p <- panel f [text := "Triggers"]
  
  btn_up <- smallButton p [ text := "^" ]
  btn_down <- smallButton p [ text := "v" ]
  btn_left <- smallButton p [ text := "<" ]
  btn_right <- smallButton p [ text := ">" ]
  btn_mid <- smallButton p [ text := "X" ]

  let cmds = zip 
        [btn_up, btn_down, btn_left, btn_right, btn_mid]
        ["up","down","left","right","mid"]
      fn (b,c) = set b [ on command := signal bus ("trigger-"++c) () ]
  mapM_ fn cmds

  set p [ layout := alignTop $ hfill (alignCenter $ grid 5 5
    [ [ glue, widget btn_up, glue ],
      [ widget btn_left, widget btn_mid, widget btn_right ],
      [ glue, widget btn_down, glue ] ] ) ]
  return p

tv_main = start $ do
  f <- frame [text := "TriggerView" ]
  bus <- initBus
  p <- triggerView bus f
  set f [ layout := WX.fill (widget p) ]



