
module Overview where


import System.Glib as Glib
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.TreeList.TreeModelSort
import qualified Graphics.UI.Gtk.ModelView as ModelView
import qualified Graphics.UI.Gtk.Glade as Glade

import Data.Word
import Data.Array
import Data.Array.MArray

import Control.Exception
import Control.Monad

import IO hiding (try)
import Text.Printf

import Data.Tree
import Monad

import Misc

import OMXNordic
import Quote
import Chart
import Price

data StockData = StockData {
    sd_info :: Stock,
    sd_data :: [Quote],
    sd_graph_200 :: (Pixbuf,Double),
    sd_graph_20 :: (Pixbuf,Double)
  }

setupStockList view model = do
  let setupColumn (n, (label, proj)) = do
        col <- ModelView.treeViewColumnNew 
        set col [ treeViewColumnTitle := label,
                  treeViewColumnResizable := True,
                  treeViewColumnSortColumnId := n ]
        ModelView.treeViewColumnSetTitle col label
        renderer <- ModelView.cellRendererTextNew
        ModelView.cellLayoutPackStart col renderer True
        ModelView.cellLayoutSetAttributes col renderer model $ \stock -> [ ModelView.cellText := proj (sd_info stock) ]
        ModelView.treeViewAppendColumn view col
        return (col,renderer)


  xs@[name, market, subMarket, category, shortName, isin, issuer, previousList] <-
    mapM setupColumn $ zip [0..]
      [("Name", stock_name),
       ("Market", stock_market),
       ("Submarket", stock_subMarket),
       ("Category", stock_category),
       ("Shortname", stock_shortName),
       ("ISIN", stock_isin),
       ("Issuer", stock_issuer),
       ("Previous list", stock_previousList)]

  col <- ModelView.treeViewColumnNew
  set col [ treeViewColumnTitle := "Graph 200" ]
  renderer <- ModelView.cellRendererPixbufNew
  ModelView.cellLayoutPackStart col renderer False
  ModelView.cellLayoutSetAttributes col renderer model $
    \stock -> [ ModelView.cellPixbuf := fst (sd_graph_200 stock) ]
  ModelView.treeViewAppendColumn view col

  col <- ModelView.treeViewColumnNew
  set col [ treeViewColumnTitle := "Range" ]
  renderer <- ModelView.cellRendererTextNew
  ModelView.cellLayoutPackStart col renderer False
  ModelView.cellLayoutSetAttributes col renderer model $
    \stock -> [ ModelView.cellText := printf "%3.2f%%" (snd (sd_graph_200 stock)) ]
  ModelView.treeViewAppendColumn view col

  col <- ModelView.treeViewColumnNew
  set col [ treeViewColumnTitle := "Graph 20" ]
  renderer <- ModelView.cellRendererPixbufNew
  ModelView.cellLayoutPackStart col renderer False
  ModelView.cellLayoutSetAttributes col renderer model $
    \stock -> [ ModelView.cellPixbuf := fst (sd_graph_20 stock) ]
  ModelView.treeViewAppendColumn view col

  col <- ModelView.treeViewColumnNew
  set col [ treeViewColumnTitle := "Range" ]
  renderer <- ModelView.cellRendererTextNew
  ModelView.cellLayoutPackStart col renderer False
  ModelView.cellLayoutSetAttributes col renderer model $
    \stock -> [ ModelView.cellText := printf "%3.2f%%" (snd (sd_graph_20 stock)) ]
  ModelView.treeViewAppendColumn view col


  return xs

mkDefaultPixbuf = do
  emptyPixbuf <- pixbufNew ColorspaceRgb True 8 200 30
  pd <- pixbufGetPixels emptyPixbuf
  stride <- pixbufGetRowstride emptyPixbuf

  forM [0..29] $ \y -> do
    forM [0..199] $ \x -> do
      writeArray pd (y * stride + 4*x + 0) (0x40::Word8)
      writeArray pd (y * stride + 4*x + 1) 0x40
      writeArray pd (y * stride + 4*x + 2) 0x70
      writeArray pd (y * stride + 4*x + 3) 0x80
  return emptyPixbuf

calcRange qs = 
  let min = minimum (map (priceToFloat . q_low) qs)
      max = maximum (map (priceToFloat . q_high) qs)
      range = max - min
  in (100 * range / min)

loadOrGenerateGraph pixmap sym render qs samples size@(width,height) highlight = do
  let (y,m,d) = q_date (last qs)
  let fn = printf "cache/%s.%d-%02d-%02d.%d.png" sym y m d samples
  r <- try $ pixbufNewFromFile fn
  case Left () {- r -} of
   Right (Right pixbuf) -> do
    printf "[Loaded %s for %d-%02d-%02d %d]" sym y m d samples
    hFlush stdout
    return (pixbuf,calcRange (takeLast samples qs))
   Left _ -> do
    printf "[Generating %s for %d-%02d-%02d %d]" sym y m d samples
    hFlush stdout
    range <- renderWithDrawable pixmap (render (takeLast samples qs) highlight width height)
    Just pixbuf <- pixbufGetFromDrawable pixmap (Rectangle 0 0 (round width) (round height))
    pixbufSave pixbuf fn "png" []
    return (pixbuf,range)

testLoadOrGenerate sym = do
  w <- windowNew
  widgetShow w
  draw <- widgetGetDrawWindow w
  pixmap <- pixmapNew (Just draw) 200 30 Nothing
  qs <- readQuoteFileForSymbol sym
  loadOrGenerateGraph pixmap sym mkSmallChart qs 200 (200,30) 20
  loadOrGenerateGraph pixmap sym mkSmallChart qs 20 (200,30) 5
  widgetDestroy w

pregenerateAll = do
  w <- windowNew
  widgetShow w
  draw <- widgetGetDrawWindow w
  pixmap <- pixmapNew (Just draw) 200 30 Nothing
  xs <- readOMXList
  forM xs $ \stock -> do
    let sym = stock_shortName stock
    dt <- try $ readQuoteFileForSymbol sym
    case dt of
     Right qs -> do
      printf "Generate graph for %s.." sym
      hFlush stdout
      (pixbuf200,range200) <- loadOrGenerateGraph pixmap sym mkSmallChart qs 200 (200,30) 20
      (pixbuf20,range20) <- loadOrGenerateGraph pixmap sym mkSmallBars qs 20 (200,30) 5
      printf "done\n"
     Left _ -> printf "Failed to read quotes for %s\n" sym
  widgetDestroy w



overviewGui = do
  
  Just xmlGui <- Glade.xmlNew "hs-finance-overview-gui.glade"
  main <- Glade.xmlGetWidget xmlGui castToWindow "main"

  filterEntry <- Glade.xmlGetWidget xmlGui castToEntry "stock-filter"
  sortEntry <- Glade.xmlGetWidget xmlGui castToEntry "stock-sort"

  list <- Glade.xmlGetWidget xmlGui castToTreeView "stocklist"

  xs <- (filter (\s -> stock_market s == "STO")) $^ readOMXList
--  xs <- readOMXList


  defaultPixbuf <- mkDefaultPixbuf
  draw <- widgetGetDrawWindow main
  pixmap <- pixmapNew (Just draw) 200 30 Nothing

  ys <- forM xs $ \stock -> do
    let sym = stock_shortName stock
    dt <- try $ readQuoteFileForSymbol sym
    case dt of
     Right qs -> do
      printf "Generate graphs for %s.." sym
      hFlush stdout
      (pixbuf200,range200) <- loadOrGenerateGraph pixmap sym mkSmallChart qs 200 (200,30) 20
      (pixbuf20,range20) <- loadOrGenerateGraph pixmap sym mkSmallBars qs 20 (200,30) 5
      printf "done\n"
      return (StockData stock qs (pixbuf200,range200) (pixbuf20,range20))
     Left _ -> return (StockData stock [] (defaultPixbuf,0) (defaultPixbuf,0))

  
  model <- ModelView.treeStoreNew (unfoldForest (\s -> (s,[])) (filter (\s -> stock_market (sd_info s) == "STO") ys))
--  sortModel <- ModelView.treeModelSortNewWithModel model

  setupStockList list (model)
  ModelView.treeViewSetModel list model

  set list [ treeViewEnableSearch := True,
             treeViewSearchColumn := 4,
             treeViewHeadersVisible := True,
             treeViewHeadersClickable := True ]







  onKeyPress main $ \Key { eventKeyName = key } -> do
    when (key == "Escape") (widgetDestroy main >> mainQuit)
    return True
  onDestroy main mainQuit

  widgetShowAll main
  mainGUI

o_main = do
  initGUI
  -- unsafeInitGUIForThreadedRTS
  overviewGui


