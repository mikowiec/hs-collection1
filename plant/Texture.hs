module Texture where


import Bits((.&.))
import GL
import GLU
import Foreign.Ptr (castPtr)

import IOExts
import Png

type Textures = [(String, TextureObject)]

texInit :: [String] -> IO Textures
texInit names = do
   -- pixelStore Unpack (Alignment 1)
   rowAlignment Unpack $= 1
   -- is <- genTextures (length names)
   is <- genObjectNames (length names)
   let ts = zip names is
   let init_tex (name,tex_id) =
        withPng ("images/"++name++".png") $ \(Png w' h' d) -> do
          let w = fromIntegral w'
              h = fromIntegral h'
          -- GL.bindTexture GL.Texture2d tex_id
          textureBinding Texture2D $= (Just tex_id)

          -- texEnv (TextureEnvMode Replace)
          textureFunction $= Replace
    {-
          mapM_ (texParameter Texture2d)
                  [ TextureWrap S Clamp, TextureWrap T Clamp,
                    TextureFilters Nearest Nearest ]
    -}
          (textureFilter Texture2D) $= ((Nearest,Nothing), Nearest)
          textureWrapMode Texture2D S $= (Repeated, Clamp)
          textureWrapMode Texture2D T $= (Repeated, Clamp)
          -- let pd = PixelDescriptor GL.Rgba UnsignedByte (castPtr d)
          let pd = PixelData RGBA UnsignedByte (castPtr d)
          -- texImage2D Texture2d 0 GL.Rgba' w h 0 pd
          texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D w h) 0 pd
   mapM_ init_tex ts
   texture Texture2D $= Enabled
   -- texEnv (TextureEnvMode Decal)
   textureFunction $= Decal
   return ts


