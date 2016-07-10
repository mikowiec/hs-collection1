
module Main where


import Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Glade as Glade

import Monad

gui = do
  
  Just xmlGui <- Glade.xmlNew "hs-finance-overview-gui.glade"
  main <- Glade.xmlGetWidget xmlGui castToWindow "main"

  onKeyPress main $ \Key { eventKeyName = key } -> do
    when (key == "Escape") (widgetDestroy main >> mainQuit)
    return True
  onDestroy main mainQuit

  widgetShowAll main
  mainGUI

main = do
  initGUI
  gui


