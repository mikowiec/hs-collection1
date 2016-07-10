
module MultiTexturing where

import qualified GL

gl_texture0_arb      :: Int
gl_texture0_arb      = 33984
gl_texture1_arb      :: Int
gl_texture1_arb      = 33985
gl_combine_arb       :: Int
gl_combine_arb       = 0x8570 
gl_combine_rgb_arb   :: Int
gl_combine_rgb_arb   = 0x8571
gl_combine_alpha_arb :: Int
gl_combine_alpha_arb = 0x8572
gl_source0_rgb_arb   :: Int
gl_source0_rgb_arb   = 0x8580
gl_source1_rgb_arb   :: Int
gl_source1_rgb_arb   = 0x8581
gl_source2_rgb_arb   :: Int
gl_source2_rgb_arb   = 0x8582
gl_source0_alpha_arb :: Int
gl_source0_alpha_arb = 0x8588
gl_source1_alpha_arb :: Int
gl_source1_alpha_arb = 0x8589
gl_source2_alpha_arb :: Int
gl_source2_alpha_arb = 0x858A
gl_operand0_rgb_arb  :: Int
gl_operand0_rgb_arb  = 0x8590
gl_operand1_rgb_arb  :: Int
gl_operand1_rgb_arb  = 0x8591
gl_operand2_rgb_arb  :: Int
gl_operand2_rgb_arb  = 0x8592
gl_operand0_alpha_arb:: Int
gl_operand0_alpha_arb= 0x8598
gl_operand1_alpha_arb:: Int
gl_operand1_alpha_arb= 0x8599
gl_operand2_alpha_arb:: Int
gl_operand2_alpha_arb= 0x859A
gl_rgb_scale_arb     :: Int
gl_rgb_scale_arb     = 0x8573
gl_add_signed_arb    :: Int
gl_add_signed_arb    = 0x8574
gl_interpolate_arb   :: Int
gl_interpolate_arb   = 0x8575
gl_subtract_arb      :: Int
gl_subtract_arb      = 0x84E7
gl_constant_arb      :: Int
gl_constant_arb      = 0x8576
gl_primary_color_arb :: Int
gl_primary_color_arb = 0x8577
gl_previous_arb      :: Int
gl_previous_arb      = 0x8578
gl_interpolate       :: Int
gl_interpolate       = 0x8575
gl_src_color         :: Int
gl_src_color         = 0x0300
gl_texture           :: Int
gl_texture           = 0x1702
gl_texture_env       :: Int
gl_texture_env       = 0x2300
gl_texture_env_mode  :: Int
gl_texture_env_mode  = 0x2200

{-
foreign import ccall glActiveTextureARB :: Int -> IO ()
foreign import ccall glMultiTexCoord2fARB :: Int -> Float -> Float -> IO ()

foreign import ccall glTexEnvi :: Int -> Int -> Int -> IO ()
-}

glActiveTextureARB _ = return ()

glMultiTexCoord2fARB :: Int -> GL.GLfloat -> GL.GLfloat -> IO ()
glMultiTexCoord2fARB _ u v =
    GL.texCoord (GL.TexCoord2 u v)

glTexEnvi _ _ _ = return ()

