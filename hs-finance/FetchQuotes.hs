
module Main where

import YahooQuotes
import Overview

import Graphics.UI.Gtk as Gtk

main = do
  initGUI
  fetchAllStockholm
  pregenerateAll


