
module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import qualified Data.ByteString as BS

import System.Exit
import Monad
import Text.Printf

import Profile

import Photo

display tex = do
  printf "display\n"
  texture Texture2D $= Enabled
  textureFunction $= Replace
  
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  textureFilter Texture2D $= ((Nearest,Nothing),Nearest)
  
  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer,DepthBuffer]
  color (Color4 1 0 0 1 :: Color4 GLfloat)
  ortho (-1.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
  
  
  renderPrimitive Quads $ do
    texCoord2 0 0
    vertex3 0.2 0.2 0
    texCoord2 1 0
    vertex3 0.8 0.2 0
    texCoord2 1 1
    vertex3 0.8 0.8 0
    texCoord2 0 1
    vertex3 0.2 0.8 0


  flush
  swapBuffers


texCoord2 :: GLfloat -> GLfloat -> IO ()
texCoord2 s t = texCoord (TexCoord2 s t)
vertex3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3 x y z = vertex (Vertex3 x y z)



reshape (Size w h) = do
  printf "reshape %d %d\n" w h
  return ()

input (Char '\ESC') _ _ _ = exitWith ExitSuccess
input key keystate mods pos = do
  print (key,keystate,mods,pos)

main = do
  getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,RGBMode,WithDepthBuffer]
  createWindow "Hello World"
  fullScreen

  Just (w,h,bs) <- parsePPM `liftM` loadPreview "raw.cr2"

  blendEquation $= FuncAdd
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  shadeModel $= Flat

  [tex] <- genObjectNames 1
  textureBinding Texture2D $= Just tex
  texBindImage (w,h,bs)

  keyboardMouseCallback $= Just input
  displayCallback $= display tex
  reshapeCallback $= Just reshape
  mainLoop

profile_main = profile_report "main" $ do
  bs <- loadPreview "raw.cr2"
  BS.writeFile "raw.ppm" bs

{-
main = do
  installHandler sigPIPE Ignore Nothing
  bs <- loadPreview "raw.cr2"
  BS.writeFile "raw.pnm" bs

-}

