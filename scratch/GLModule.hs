
module GLModule where

import Data.IORef

data GLMODULE a = GLMODULE {
  glm_data :: IORef (Maybe a),
  glm_create :: IO a,
  glm_display :: a -> IO (),
  glm_resize :: a -> Int -> Int -> IO ()
 }

data GLModule = forall a . GLModule (GLMODULE a)

gl_create (GLModule (GLMODULE ref create display resize)) = do
  d <- create
  writeIORef ref (Just d)

gl_display (GLModule (GLMODULE ref create display resize)) = do
  Just d <- readIORef ref
  display d

gl_resize (GLModule (GLMODULE ref create display resize)) w h = do
  Just d <- readIORef ref
  resize d w h



