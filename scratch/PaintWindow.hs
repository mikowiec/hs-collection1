
module PaintWindow where

import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WX


paint_window :: Window a -> IO (Window ())
paint_window f
  = window f [ on paint := onpaint, fullRepaintOnResize := True]
  where
    onpaint dc viewArea
      = do circle dc (pt 200 200) 20 [penKind := PenDash DashDot]
           arc dc (pt 100 100) 20 90 230 [WX.color := red, penWidth :~ (+1), penKind := PenSolid]
           ellipticArc dc (WX.rect  (pt 20  20) (sz 60 30)) 90 230 [WX.color := blue, penWidth :~ (*2)]
           c <- WX.get dc WX.color
           -- set dc [font := fontDefault{ fontFace = "Courier New", fontSize = 16, fontWeight = WeightBold }]
           WX.set dc [fontFace := "Courier New", fontSize := 16, fontWeight := WeightBold ]
           drawText dc (show c) (pt 50 50) []
           rotatedText dc "rotated text" (pt 80 160) 45 [textColor := green]

