
module Scene (scene, actions) where

import KastorScene
import KastorSceneNodeOp
import KTypes


scene :: Scene

actions :: Actions SVG



scene = 
  node svg
   [width 240, height 320,
    node image
     [x 0, y 0, width 240, height 320, href "jungle.png",
      node pixeleffect
       [x 20, y 130, width 200, height 30,
        effect_type KID_desaturate, opacity (Int16x16 0 0xf000)],
      node kerneleffect
       [x 20, y 150, width 200, height 30,
        effect_type KID_blur, opacity (Int16x16 0 0x8000)] ],
    node rect
     [x 10, 
      y 20,
      width 50,
      height 30,
      cfill (color "red"),
      bind "red_x" x] ,
    node rect
     [x 20, y 100,
      width 20, height 20,
      cfill (color "blue"),
      node_id (MkAttrString "blue_rect")],
    node image
     [x 20, y 200, width 50, height 50, href "tatman_transparent.png",
      node animate
       [attributeName KID_x,
        from [20], to [180],
        fill KID_freeze, 
        begin (MkAttrTimeMS 2000), dur (MkAttrTimeMS 2000)],
    node particles
     [node emitter
       [shape KID_circle,
        x0 120, y0 160, x1 140, y1 180,
        intensity 20,
        href "tatman_transparent.png",
        particlewidth 8, particleheight 8,
        angle 90,
        anglespread 360,
        velocity 10,
        velocityspread 5,
        lifespan 3,
        lifespanspread 1,
        fadeindur 1,
        fadeoutdur 2
      ] ] ] ]

actions =
 [("right", [(NodeWithName "blue_rect",
    node animate [attributeName KID_x, from [12], to [40], 
                   fill KID_freeze,
                   begin (MkAttrTimeMS 100), dur (MkAttrTimeMS 2000)  ] )] )]



