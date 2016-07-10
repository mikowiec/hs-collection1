
module Chart where

import Debug.Trace

import Data.IORef

import Graphics.UI.Gtk as G
import Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.UI.Gtk.Pango.Font as PangoFont
import qualified Graphics.UI.Gtk.Glade as Glade

import System.Glib (handleGError, GError(..))

import System

import Text.Printf
import Control.Monad
import List

import Misc

import App.Signals

import Quote
import Price

data GraphSetup = GraphSetup {
  graph_cursor_index :: Int,  -- index from beginning of quote list
  graph_cursor_pos :: Double, -- rel x position [0..1]
  graph_visible_ticks :: Int, 
  graph_cross :: (Double,Double),
  graph_screenToSheetMatrix :: Matrix.Matrix,
  graph_screenToGridRel :: Double -> Double,
  graph_select_to :: Maybe Int
 }


renderGraph sheetMatrix gs qs = do
  setMatrix Matrix.identity
  setSourceRGBA 0 0 0 1
  let (v:vs) = mapsnd (priceToFloat . q_close) qs
  let f (x,y) = Matrix.transformPoint sheetMatrix (x,y)
  uncurry moveTo (f (v))
  mapM_ (uncurry lineTo . f) vs
  stroke

mkSmallChart qs sh w h = do
  setMatrix Matrix.identity
  setSourceRGBA 1 1 1 1
  rectangle 0 0 w h
  C.fill
  setSourceRGBA 0.8 0.8 0.8 1
  rectangle (w-sh) 0 w h
  C.fill
  setSourceRGBA 0.3 0.3 0.3 1
  setLineWidth 1
  moveTo 0 0
  mapM_ (uncurry lineTo) [(w,0),(w,h),(0,h),(0,0)]
  stroke
  setSourceRGBA 0 0 0 1
  let vs = mapsnd (priceToFloat . q_close) (zip [w-1,w-2..] (reverse qs))
  let min = minimum (map snd vs)
      max = maximum (map snd vs)
      range = max - min
  let (v:vs') = mapsnd (\v -> h - ((v - min) / range * h)) vs
  uncurry moveTo v
  mapM_ (uncurry lineTo) vs'
  stroke
  return (100 * range / min)

mkSmallBars qs sh w h = do
  setMatrix Matrix.identity
  setSourceRGBA 1 1 1 1
  rectangle 0 0 w h
  C.fill
  setSourceRGBA 0.8 0.8 0.8 1
  rectangle (w-(10.5*sh)) 0 w h
  C.fill
  setSourceRGBA 0 0 0 1
  setLineWidth 1
  moveTo 0 0
  mapM_ (uncurry lineTo) [(w,0),(w,h),(0,h),(0,0)]
  stroke
  setSourceRGBA 0 0 0 1

  let min = minimum (map (priceToFloat . q_low) qs)
      max = maximum (map (priceToFloat . q_high) qs)
      range = max - min
  let f v = h - (priceToFloat v - min) / range * h

  forM (zip [1,11..] qs) $ \(x,q) -> do
    moveTo x (f (q_low q))
    lineTo x (f (q_high q))
    moveTo (x-3) (f (q_open q))
    lineTo x (f (q_open q))
    moveTo x (f (q_close q))
    lineTo (x+3) (f (q_close q))
  stroke

  return (100 * range / min)

mkSmallCandle qs sh w h = do
  setMatrix Matrix.identity
  setSourceRGBA 1 1 1 1
  rectangle 0 0 w h
  C.fill
  setSourceRGBA 0.8 0.8 0.8 1
  rectangle (w-(10.5*sh)) 0 w h
  C.fill
  setSourceRGBA 0 0 0 1
  setLineWidth 1
  moveTo 0 0
  mapM_ (uncurry lineTo) [(w,0),(w,h),(0,h),(0,0)]
  stroke
  setSourceRGBA 0 0 0 1

  let mini = minimum (map (priceToFloat . q_low) qs)
      maxi = maximum (map (priceToFloat . q_high) qs)
      range = maxi - mini
  let f v = h - (priceToFloat v - mini) / range * h

  forM (zip [1,11..] qs) $ \(x,q) -> do
    let body_bot = min (q_open q) (q_close q)
        body_top = max (q_open q) (q_close q)
    moveTo x (f (q_low q))
    lineTo x (f body_bot)
    moveTo x (f (q_high q))
    lineTo x (f body_top)
    stroke
    rectangle (x-3) (f body_top) 7 (f body_bot - f body_top)
    if priceToFloat (q_open q) < priceToFloat (q_close q)
      then stroke
      else C.fill
  return (100 * range / mini)

viewChart render sym = do
  qs <- takeLast 40 $^ readQuoteFileForSymbol sym
  let width = 400
      height = 100
  window <- windowNew
  windowSetDefaultSize window width height
  onExpose window $ \_ -> do
    draw <- widgetGetDrawWindow window
    renderWithDrawable draw (render qs (fromIntegral (length qs `div` 4)) (fromIntegral width) (fromIntegral height))
    return True
  onDestroy window mainQuit
  widgetShow window
  mainGUI


mkTicks :: Double -> Double -> Int -> [Double]
mkTicks low high n =
    case drop n (findLabels (high - low)) of
      (step:_) ->
        let min = step * fromIntegral (floor (low / step))
            max = step * fromIntegral (ceiling (high / step))
        in [min,min+step..max]
      _ -> [low,high]

labels = [ 1000, 500, 200, 100, 50, 10, 5, 1, 0.5, 0.1, 0.01 ]
findLabels range = dropWhile (>range) labels

scaledBy i w = round $ fromIntegral i * w

draw canvas graphSetupRef qs = do
  (width, height) <- widgetGetSize canvas
  let fWidth = fromIntegral width
      fHeight = fromIntegral height
  window <- widgetGetDrawWindow canvas

  let n = length qs


  gs@(GraphSetup cursor_index cursor_pos visible_ticks cross@(cross_x,cross_y) _ _ select_to) <- readIORef graphSetupRef

  let minIndex = fromIntegral cursor_index - cursor_pos * fromIntegral visible_ticks
  let maxIndex = minIndex + fromIntegral visible_ticks

  let cs = take visible_ticks (drop (round minIndex) ((map (priceToFloat . q_close) qs)))
      minClose = minimum cs :: Double
      maxClose = maximum cs


  let graphWidth = fWidth - 200
      graphHeight = fHeight - 100
      graphX = 100
      graphY = 50
  

  let scaleX = graphWidth / (fromIntegral visible_ticks)

  let gridMatrix =
        (Matrix.translate 0 fHeight
          (Matrix.scale 1 (-1)
            (Matrix.translate graphX graphY
              Matrix.identity)))

  let sheetMatrix = 
        (Matrix.translate 0 fHeight
          (Matrix.scale 1 (-1)
            (Matrix.translate graphX graphY
              (Matrix.translate (cursor_pos * scaleX * fromIntegral visible_ticks) 0
                (Matrix.scale scaleX 1
                  (Matrix.translate (negate (fromIntegral cursor_index)) 0
                    (Matrix.translate 0 20
                      (Matrix.scale 1 ((graphHeight-40) / ((maxClose - minClose)))
                        (Matrix.translate 0 (negate minClose)
                          Matrix.identity)))))))))

  modifyIORef graphSetupRef $ \gs ->
    gs { graph_screenToSheetMatrix = Matrix.invert sheetMatrix,
         graph_screenToGridRel = \x -> (fst (Matrix.transformPoint (Matrix.invert gridMatrix) (x,0))) / graphWidth }

  let (cross_x',cross_y') = Matrix.transformPoint (Matrix.invert sheetMatrix) cross

  let cross' = Matrix.transformPoint (Matrix.invert sheetMatrix) cross

  let s2s_y v = snd (Matrix.transformPoint sheetMatrix (0,v))
  let s2s_x v = fst (Matrix.transformPoint sheetMatrix (v,0))

  renderWithDrawable window $ do
    screenMatrix <- getMatrix
    let showText string = do
          save
          setMatrix screenMatrix
          pl <- createLayout string
          showLayout pl
          restore

    resetClip
    setMatrix gridMatrix

    setLineWidth 1
    setSourceRGBA 0 0 0 1
    moveTo 0 0
    mapM (uncurry lineTo) [(graphWidth,0),(graphWidth,graphHeight),(0,graphHeight),(0,0)]
    stroke

    setSourceRGBA 1 1 1 1
    rectangle 0 0 graphWidth graphHeight
    C.fillPreserve
    clip

    maybeM select_to $ \to -> do
      setMatrix Matrix.identity
      setSourceRGBA 0.9 0.9 0.9 1
      let x1 = s2s_x (fromIntegral cursor_index); x2 = s2s_x (fromIntegral to)
      rectangle x1 graphY (x2-x1) (graphY+graphHeight)
      C.fill

    setSourceRGBA 0 0 0 1

    setLineWidth 1
    renderGraph sheetMatrix gs (take visible_ticks (drop (round minIndex) (zip [0..] qs)))

    resetClip

    setMatrix Matrix.identity

    setSourceRGBA 0 0 0 1
    setLineWidth 1

    setMatrix gridMatrix
    rectangle 0 0 (negate graphX) graphHeight
    clip

    setMatrix Matrix.identity


    forM (mkTicks minClose maxClose 1) $ \v -> do
      let y = s2s_y v
      moveTo graphX y
      lineTo (graphX - 10) y
      moveTo (graphX - 60) (y - 10)
      showText (show v)
    forM (mkTicks minClose maxClose 2) $ \v -> do
      let y = s2s_y v
      moveTo graphX y
      lineTo (graphX - 4) y

    stroke

    resetClip
    setMatrix gridMatrix
    rectangle 0 0 graphWidth (negate graphY)
    clip

    setMatrix Matrix.identity
    forM (mkTicks minIndex maxIndex 1) $ \v -> do
      let x = s2s_x v
      moveTo x (graphY+graphHeight)
      lineTo x (graphY + graphHeight + 20)
      moveTo x (graphY + graphHeight + 20)
      case splitAt (round v) qs of
        (_,(Quote { q_date = (y,m,d) }:_)) -> showText (printf "%4d-%02d-%02d" y m d)
        _ -> return ()

    stroke

    resetClip

    setSourceRGBA 0.8 0.8 0.8 1
    setMatrix Matrix.identity

    rectangle graphX graphY graphWidth graphHeight
    clip

    moveTo graphX (s2s_y cross_y')
    lineTo (graphX + graphWidth) (s2s_y cross_y')
    moveTo (s2s_x cross_x') graphY
    lineTo (s2s_x cross_x') (graphY + graphHeight)
    stroke

    setSourceRGBA 1 0 0 1
    moveTo (s2s_x (fromIntegral cursor_index)) graphY
    lineTo (s2s_x (fromIntegral cursor_index)) (graphY + graphHeight)
    stroke

  return True


{-

   labels
   from <-> to
   ticks
   slider, zoom
   "trace32" interaction
   select graphs
   add derivates
   (tickbox)
   parameters for derivates, global, local adjustment
   derivate manager

 -}


pixelToPercent canvas x = do
  (width, height) <- widgetGetSize canvas
  return (100 * x / fromIntegral width)
  
gui stock = do

  bus <- initBus

  qs <- readQuoteFile ("quotes/"++stock++".dk")
  
  {- create -}
  Just xmlGui <- Glade.xmlNew "hs-finance-graph-gui.glade"

  window <- Glade.xmlGetWidget xmlGui castToWindow "main"
  canvas <- Glade.xmlGetWidget xmlGui castToDrawingArea "graph"
  
  {- connect -}
  onKeyPress window $ \Key { eventKeyName = key } -> do
    when (key == "Escape") (widgetDestroy window >> mainQuit)
    return True
  onDestroy window mainQuit

  symbol <- Glade.xmlGetWidget xmlGui castToEntry "symbol"
  set symbol [ entryText := stock ]

  time_scale <- Glade.xmlGetWidget xmlGui castToScrollbar "time-scale"
  cursor_idx <- Glade.xmlGetWidget xmlGui castToScrollbar "cursor"
  reset <- Glade.xmlGetWidget xmlGui castToButton "reset"

  zoom <- get time_scale rangeValue
  let graphSetup = (GraphSetup (length qs) 1.0 (length qs) (0,0) Matrix.identity id Nothing)
  graphSetupRef <- newIORef graphSetup

  onRangeValueChanged time_scale $ do
    val <- get time_scale rangeValue
    signal bus "time-scale" val

  onRangeValueChanged cursor_idx $ do
    val <- get cursor_idx rangeValue
    signal bus "cursor-index" val

  monitor bus "time-scale" $ \val -> do
    let _ = val :: Double
    modifyIORef graphSetupRef $ \gs ->
      gs { graph_visible_ticks = (round (val * genericLength qs / 100)) }
    widgetQueueDraw canvas

  monitor bus "cursor-index" $ \val -> do
    let _ = val :: Double
    modifyIORef graphSetupRef $ \gs ->
      gs { graph_cursor_index = (round (val * genericLength qs / 100)) }
    widgetQueueDraw canvas

  onClicked reset $ do
    writeIORef graphSetupRef graphSetup
    widgetQueueDraw canvas

  monitor bus "time-scale-zoom-delta" $ \dz -> do
    v <- get time_scale rangeValue
    set time_scale [ rangeValue := v + 5 * fromIntegral (dz::Int) ]

  monitor bus "graph-cross-move" $ \cross -> do
    modifyIORef graphSetupRef $ \gs -> gs { graph_cross = cross }
    widgetQueueDraw canvas

  onMotionNotify canvas False $ \(Motion _sent _timestamp x y mod _hint _xroot _yroot) -> do
--    writeIORef cursorRef (x,y)
    gs <- readIORef graphSetupRef
    let (x',_) = Matrix.transformPoint ((graph_screenToSheetMatrix gs)) (x,0)

    maybeM (graph_select_to gs) $ \_ -> do
      modifyIORef graphSetupRef $ \gs ->
        gs { graph_select_to = Just (round x') }

    signal bus "graph-cross-move" (x,y)
    return True

  onButtonPress canvas $ \(Button _sent _click _timestamp x _y _mod button _xroot _yroot) -> do
    gs <- readIORef graphSetupRef
    let (x',_) = Matrix.transformPoint ((graph_screenToSheetMatrix gs)) (x,0)

    modifyIORef graphSetupRef $ \gs -> gs { graph_select_to = Just (round x') }

    case button of
     RightButton -> writeIORef graphSetupRef graphSetup
     
     LeftButton -> do
      modifyIORef graphSetupRef $ \gs ->
        gs { graph_cursor_index = round x', graph_cursor_pos = graph_screenToGridRel gs x }

     _ -> return ()
    
    return True
  
  onButtonRelease canvas $ \(Button _sent _click _timestamp x _y _mod button _xroot _yroot) -> do
    gs <- readIORef graphSetupRef
    let (x',_) = Matrix.transformPoint ((graph_screenToSheetMatrix gs)) (x,0)
    
    case button of
     RightButton -> do
      writeIORef graphSetupRef graphSetup
      set time_scale [ rangeValue := 100 ]
     
     LeftButton -> do
      maybeM_ (graph_select_to gs) $ \s1 -> do
       let s2 = graph_cursor_index gs
       when (s1 /= s2) $ do
        let from = min s1 s2
            to = max s1 s2
        modifyIORef graphSetupRef $ \gs ->
          gs { graph_cursor_pos = if s1 < s2 then 0.9 else 0.1 }
        set time_scale [ rangeValue := fromIntegral (to - from) / genericLength qs * 100 ]
        set cursor_idx [ rangeValue := fromIntegral (graph_cursor_index gs) / genericLength qs * 100 ]

     _ -> return ()

    modifyIORef graphSetupRef $ \gs ->
          gs { graph_select_to = Nothing }
    widgetQueueDraw canvas
    return True
  
  onScroll canvas $ \(Scroll _sent _timestamp _x _y direction _xroot _yroot) -> do
    case fromEnum direction of
      0 -> signal bus "time-scale-zoom-delta" (-1 :: Int)
      1 -> signal bus "time-scale-zoom-delta" (1 :: Int)
      _ -> return ()
    return True

  onExpose canvas $ \_ -> do
    (width, height) <- widgetGetSize canvas
--    modifyIORef graphSetupRef $ \gs -> gs { graph_screenToSheetMatrix = mkSheetMatrix width height }
    draw canvas graphSetupRef qs


  {- setup -}
  widgetSetSizeRequest window 640 480

  widgetShowAll window
  mainGUI

initChart = initGUI

t = gui "MIC_SDB"

c_main = do
  (a:as) <- getArgs
  initGUI
  gui a


