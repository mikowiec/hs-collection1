module ScriptEditView where


import Graphics.UI.WX as WX
import Signals


scriptEditView bus f = do
  p <- panel f []
  w_scr <- textCtrl p [ size := sz 200 200, font := fontFixed ]

  w_lbl <- staticText p [ text := "Module:" ]
  w_fn <- staticText p [ font := fontFixed ]

  let reload = do
       fn <- get w_fn text
       code <- get w_scr text
       signal bus "reload-code" (fn,code)
  w_reload <- button p [ text := "Reload", on command := reload ]

  rm <- monitor bus "edit-code" $
    \(fn,code) -> do set w_scr [ text := code ]
                     set w_fn [ text := fn ]

  set p [ layout := fill $ 
            column 5 [row 5 [alignBottom $ widget w_reload,
                             alignBottom $ widget w_lbl,
                             alignBottom $ widget w_fn,
                             glue],
                      fill $ widget w_scr]]
  set p [ on closing :~ \c -> do rm ; c ]
  return p


sev_main = start $ do
  f <- frame []
  bus <- initBus
  scriptEditView bus f

