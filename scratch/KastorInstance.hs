
module KastorInstance where

import KInterface

import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.IORef
import Monad
import Numeric
import Control.Exception
import Text.Printf
import Profile

data KastorInst = KI KCore (Ptr Word16)
data Kastor = Kastor (IORef (Maybe KastorInst))


createKastor :: Int -> Int -> IO Kastor
createKastor w h = do
  core <- kcore_create 0
  fb <- mallocBytes (2*w*h)
  mapM_ (\i -> pokeElemOff fb i (0::Word16)) [0..w*h-1]
  kcore_setRenderMethod core RenderSoftware w h (castPtr fb) ColorFormat_RGB_565
  ref <- newIORef (Just (KI core fb))
  return (Kastor ref)


destroyKastor (Kastor ref) = do
  Just (KI core fb) <- atomicModifyIORef ref (\k -> (Nothing,k))
  kcore_release core
  free fb
  return ()

kastorRun (Kastor ref) now update = try $ do
  Just (KI core fb) <- readIORef ref
  let time_now = fromIntegral now `div` 1000
--  putStrLn ("Kastor run at " ++ show time_now)
  kcore_setCurrentTime core ((fromIntegral now `div` 1000))
  KNode root <- kcore_getRoot core
  when (root /= nullPtr) $ do
    next <- kcore_run core
--  when (next >= 0) $ putStrLn ("Repaint in " ++ show next)
    r <- kcore_getLastRenderResult core
    unless r $ putStrLn "Kastor Run failed!"
    dr <- kcore_getDirtyRects core 10
    when (next >= 0 || not (null dr)) $
        {-profile_report "update" $ -} update fb dr next


withKastor (Kastor ref) f = do
  Just (KI core fb) <- readIORef ref
  f core



