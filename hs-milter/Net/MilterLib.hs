
module Net.MilterLib where

import Prelude hiding (catch)
import IO hiding (catch, bracket, try)
import Control.Concurrent
import Control.Exception

import Foreign
import Foreign.StablePtr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Data.Array.IO

import Data.PackedString

import Monad

import Data.IORef

import System
import Directory

import Misc
import Net.Talk

import Text.Printf

import Logger

import Control.Monad.State
import Control.Monad.Reader

import Net.Milter

import Sys.Syslog


res2int Continue = 0
res2int Reject = 1
res2int Discard = 2
res2int Accept = 3
res2int Tempfail = 4

mi_success, mi_failure :: Int
mi_success = 0
mi_failure = -1
    
data Ctx = Ctx
type Filter s = Cmd -> MilterMonad s Result

type MilterMonad s a = StateT s (ReaderT (Ptr Ctx) IO) a

runMilterM m r s = runReaderT (runStateT m s) r

ref :: IORef (Filter s)
ref = unsafePerformIO $ newIORef undefined

initialize p_ctx = do
    f <- readIORef ref
    sp <- newStablePtr (f,undefined)
    p <- malloc
    poke p sp
    smfi_setpriv p_ctx p

cleanup p_ctx = do
    p <- smfi_getpriv p_ctx
    sp <- peek p 
    freeStablePtr sp
    free p
    smfi_setpriv p_ctx (nullPtr :: Ptr ())

with_cmd :: Ptr Ctx -> Cmd -> IO Result
with_cmd p_ctx cmd = do
    p <- smfi_getpriv p_ctx
    sp <- peek p
    (f,s) <- deRefStablePtr sp
    (a,s') <- runMilterM (f cmd) p_ctx s
    sp' <- newStablePtr (f,s')
    poke p sp'
    freeStablePtr sp
    return a
  `catch` (\e->print e >> return Continue)

getsymval :: String -> MilterMonad s (Maybe String)
getsymval s = do
    p_ctx <- ask
    v <- liftIO $ s `withCString` smfi_getsymval p_ctx
    if v == nullPtr then return Nothing
     else liftIO $ Just $^ peekCString v

addheader :: String -> String -> MilterMonad s ()
addheader l v = do
    p_ctx <- ask
    liftIO $
     l `withCString` \l' -> 
      v `withCString` \v' -> do
       r <- smfi_addheader p_ctx l' v'
       when (r==mi_failure) $ fail (printf "smfi_addheader \"%s\" \"%s\" failed" l v)
    


wrap m = res2int $^ (m `catch` (\e -> try (syslog log_mail (show e)) >> return Accept))

mw_connect p_ctx cs sa = wrap $ do
    initialize p_ctx
    hn <- peekCString cs
    "255.255.255.255" `withCString` \tmp -> do
        addr_of_sockaddr tmp 16 sa
        ha <- peekCString tmp
        with_cmd p_ctx (Connect hn ha)


mw_helo p_ctx cs = wrap $ do
    s <- peekCString cs
    with_cmd p_ctx (Helo s)

mw_envfrom p_ctx csa = wrap $ do
    ps <- peekArray0 nullPtr csa
    css <- mapM peekCString ps
    with_cmd p_ctx (EnvFrom css)

mw_envrcpt p_ctx csa = wrap $ do
    ps <- peekArray0 nullPtr csa
    css <- mapM peekCString ps
    with_cmd p_ctx (EnvRcpt css)

mw_header p_ctx hl ha = wrap $ do
    sl <- peekCString hl
    sv <- peekCString ha
    with_cmd p_ctx (Header sl sv)

mw_eoh p_ctx = wrap $ do
    with_cmd p_ctx EOH

mw_body p_ctx bs bl = wrap $ do
    let m = do
             arr <- newArray_ (0,bl-1) :: IO (IOUArray Int Word8)
             foreachM [0..bl-1] $ \i -> do
               b <- peekByteOff bs i
               writeArray arr i b
             arr' <- unsafeFreeze arr 
             return arr'
    with_cmd p_ctx (Body (B m))

mw_eom p_ctx = wrap $ do
    with_cmd p_ctx EOM
    

mw_abort p_ctx = wrap $ do
    with_cmd p_ctx Abort
    

mw_close p_ctx = wrap $ do
    r <- with_cmd p_ctx Close
    cleanup p_ctx
    return r

run_milter :: String -> String -> Filter s -> IO a
run_milter name sock filter = do
    try $ removeFile $ sock
    writeIORef ref (filter)
    name `withCString` \name' ->
     sock `withCString` \sock' -> do
        r <- mw_run_filter name' sock'
        if r == 0 then exitWith ExitSuccess
         else exitWith (ExitFailure r)
    



data FilterDesc = FilterDesc
data SockAddr = SockAddr


foreign import ccall safe smfi_register    :: Ptr FilterDesc -> IO Int
foreign import ccall safe smfi_setconn     :: CString -> IO Int
foreign import ccall safe smfi_main        :: IO Int
foreign import ccall safe smfi_getpriv     :: Ptr Ctx -> IO (Ptr a)
foreign import ccall safe smfi_setpriv     :: Ptr Ctx -> Ptr a -> IO Int
foreign import ccall safe smfi_setreply    :: Ptr Ctx -> CString -> CString -> CString -> IO Int
foreign import ccall safe smfi_addheader   :: Ptr Ctx -> CString -> CString -> IO Int
foreign import ccall safe smfi_replacebody :: Ptr Ctx -> CString -> Int -> IO Int

foreign import ccall safe smfi_getsymval   :: Ptr Ctx -> CString -> IO CString

foreign import ccall safe mw_run_filter :: CString -> CString -> IO Int

foreign export ccall mw_connect :: Ptr Ctx -> CString -> Ptr SockAddr -> IO Int
foreign export ccall mw_helo    :: Ptr Ctx -> CString -> IO Int
foreign export ccall mw_envfrom :: Ptr Ctx -> Ptr CString -> IO Int
foreign export ccall mw_envrcpt :: Ptr Ctx -> Ptr CString -> IO Int
foreign export ccall mw_header  :: Ptr Ctx -> CString -> CString -> IO Int
foreign export ccall mw_eoh     :: Ptr Ctx -> IO Int
foreign export ccall mw_body    :: Ptr Ctx -> CString -> Int -> IO Int
foreign export ccall mw_eom     :: Ptr Ctx -> IO Int
foreign export ccall mw_abort   :: Ptr Ctx -> IO Int
foreign export ccall mw_close   :: Ptr Ctx -> IO Int

foreign import ccall safe addr_of_sockaddr :: CString -> Int -> Ptr a -> IO ()


