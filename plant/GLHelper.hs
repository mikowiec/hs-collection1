
module GLHelper where

import qualified GL
import qualified GLU

import MultiTexturing

translate (x,y,z) = GL.translate (GL.Vector3 x y z)
color (r,g,b,a) = GL.color (GL.Color4 r g b a :: GL.Color4 GL.GLfloat)


vertex3 :: (Float, Float, Float) -> IO ()
vertex3 = GL.vertex . ver3

ver3 (x,y,z) = GL.Vertex3 x y z
vec3 (x,y,z) = GL.Vector3 x y z

tex2 :: (GL.GLfloat, GL.GLfloat) -> IO ()
tex2 (u,v) = GL.texCoord (GL.TexCoord2 u v)

m2tex2 (u,v) = do
    glMultiTexCoord2fARB 0 u v
    glMultiTexCoord2fARB 1 u v



with_push_mtx p = do
    GL.preservingMatrix p



-----------------------------------------------------------------------------
-- NOTE: these differ between HOpenGL 1.03 and 1.04!
{-

cylinder r1 r2 h sl st = GLU.newQuadric >>= \q -> GLU.cylinder q r1 r2 h sl st

sphere r sl st = GLU.newQuadric >>= \q -> GLU.sphere q r sl st

get_viewport = do
    (wp@(GL.WindowPosition wx wy), ws@(GL.WindowSize ww wh)) <- GL.get GL.VarViewport
    return (wp,ws)

get_vp_size (GL.WindowSize w h) = (w,h)

mk_vp_pos x y = GL.WindowPosition x y
mk_vp_size w h = GL.WindowSize w h

-}
cylinder r1 r2 h sl st = 
    GLU.renderQuadric flat_style (GLU.Cylinder r1 r2 h sl st)

sphere r sl st = 
    GLU.renderQuadric flat_style (GLU.Sphere r sl st)

flat_style = GLU.QuadricStyle Nothing GLU.NoTextureCoordinates GLU.Outside GLU.FillStyle


get_viewport :: IO ((GL.GLint,GL.GLint),(GL.GLsizei,GL.GLsizei))
get_viewport = do
    (GL.Position x y, GL.Size w h) <- GL.get GL.viewport
    return ((x,y),(w,h))

get_vp_size = id

mk_vp_pos = GL.Position

mk_vp_size = GL.Size




-----------------------------------------------------------------------------



