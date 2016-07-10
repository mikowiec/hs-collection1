
{-# OPTIONS -fffi #-}

module Sys.Temp (
    temp_dir,
    temp_file,
    with_temp_file,
    cleanup,
    cleanup_rec,
    run
  ) where

import Misc

import IO
import System
import Directory
import Random
import Monad

import Foreign

import System.Posix.Process
import System.Posix.User
-- import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..))
import Foreign.C.String
import Foreign.C.Types
import System.IO ( openBinaryTempFile )
import System.Posix.Files ( setFileMode )

temp_file :: IO (String, Handle)
temp_file = mkstemp

with_temp_file :: ((String,Handle) -> IO a) -> IO a
with_temp_file m = bracket mkstemp (\(s,h) -> hClose h >> removeFile s) m

{-# NOINLINE temp_dir #-}
temp_dir :: String
temp_dir = unsafePerformIO $ do
    create_dir 10

cleanup :: IO ()
cleanup = do
    removeDirectory temp_dir `catch` \e -> print e

cleanup_rec :: IO ()
cleanup_rec = do
    xs <- getDirectoryContents temp_dir
    mapM (try . removeFile . ((temp_dir++"/")++)) xs
    cleanup


create_dir 0 = fail "cannot create temporary directory"
create_dir n = try_create_dir `catch` \_-> create_dir (pred n)

try_create_dir :: IO String
try_create_dir = do
    pn <- getProgName
    uid <- show $^ getRealUserID
    pid <- show $^ getProcessID
    tmp <- getEnv "TMP" `catch` \_->
           getEnv "TMPDIR" `catch` \_->
           return "/tmp"
    let tmpl = tmp ++ "/" ++ "haskell" ++ "_" ++ pn ++ "_" ++
               uid ++ "_" ++ pid ++ "_XXXXXX"
    mkdtemp tmpl

-- fallback
mkdtemp s = do
    rs <- sequence (replicate 6 (randomRIO ('a','z')))
    let path = (++rs) $ reverse $ drop 6 $ reverse s
    createDirectory path
    setFileMode path 0o700 `catch` (\e -> removeDirectory path >> ioError e)
    return path

{-
foreign import ccall "stdlib.h mkdtemp" c_mkdtemp :: CString -> IO CString

mkdtemp s = do
    withCString s $ \p -> do
        p' <- c_mkdtemp p
        when (p' == nullPtr) $ fail ("c_mkdtemp error: " ++ s)
        peekCString p'
-}

{-
-- fallback, unsafe
mkstemp = do
    rs <- sequence (replicate 10 (randomRIO ('a','z')))
    let path = temp_dir ++ "/hs_" ++ rs
    h <- openFile path ReadWriteMode
    return (h, rs)
-}

{-
foreign import ccall "stdlib.h mkstemp" c_mkstemp :: CString -> IO CInt

mkstemp = do
    let tmpl = temp_dir ++ "/hs_" ++ "_XXXXXX"
    withCString tmpl $ \p -> do
        fd <- c_mkstemp p
        when (fd == -1) $ fail $ "c_mkstemp failed on: " ++ tmpl
        s <- peekCString p
        h <- fdToHandle (Fd fd)
        return (h,s)
-}

mkstemp = do
    openBinaryTempFile temp_dir "hs_"


run m = (m >> cleanup) `catch` (\e -> cleanup >> ioError e)

