
module Net.MilterTalk where

import Data.IORef
import Maybe
import System.Posix
import Control.Exception
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

import System.IO
import System.Directory

import Network
import Network.BSD

import Net.Milter
import Net.MilterProtocol
import Net.MilterProtocolIO

import Text.Printf

import Misc

data Ctx = Ctx
type Filter s = Cmd -> MilterMonad s Result

type Macros = [(String,String)]

type MilterMonad s a = StateT (s) (StateT Macros IO) a


getsymval :: String -> MilterMonad s (Maybe String)
getsymval sym = do
  ms <- lift $ get
  return (lookup sym ms)

getmacros :: MilterMonad s [(String,String)]
getmacros = lift $ get

handle_client :: Handle -> Filter s -> s -> IO ()
handle_client h filter init_state = do
  runStateT (do
    runStateT (handle_client' h filter) init_state
    return ()) []
  return ()

handle_client' :: Handle -> Filter s -> MilterMonad s ()
handle_client' h filter = do
 p@(Packet cmd arr) <- liftIO $ readPacket h 
 let c = packetToCommand p
 let runCmd filter cmd = do
      (a) <- filter cmd
      liftIO $ writePacket h (responseToPacket (SMFIR_Continue))
      return True
 goon <- case c of
  SMFIC_OptNeg ver actions protocols -> do
    let resp = responseToPacket (SMFIR_OptNeg ver actions [])
    liftIO $ writePacket h resp
    return True
  SMFIC_Connect host family port address ->
    runCmd filter (Connect host address)
  SMFIC_Helo h -> runCmd filter (Helo h)
  SMFIC_Mail s ss -> runCmd filter (EnvFrom (s:ss))
  SMFIC_Rcpt s ss -> runCmd filter (EnvRcpt (s:ss))
  SMFIC_Header l v -> runCmd filter (Header l v)
  SMFIC_Eoh -> runCmd filter EOH
  SMFIC_Body arr -> runCmd filter (Body (B (return arr)))
  SMFIC_BodyEob -> runCmd filter EOM
  SMFIC_Quit -> do runCmd filter Close
                   return False
  SMFIC_Macro _ defs -> do
    lift $ modify (defs++)
    liftIO $ writePacket h (responseToPacket (SMFIR_Continue))
    return True
  SMFIC_Abort -> filter Abort >> return True -- TODO: reinit
 liftIO $ try $ hFlush h
 when goon $ handle_client' h filter

run_milter :: String -> String -> (Net.Milter.Cmd -> MilterMonad s Result) -> s -> (s -> IO s) -> IO ()
run_milter name socket filter init_state update_state = do
  installHandler sigPIPE Ignore Nothing
  sock <- listenOn (UnixSocket socket)
  let loop state = do
       (h,hn,pn) <- accept sock
       state' <- update_state state
       try (forkIO (do r <- try (handle_client h filter state' `finally` (try (hClose h)))
                       when (isLeft r) $ print (fromLeft r)))
       loop state'
  loop init_state
  sClose sock
 `finally` (try $ removeFile socket)

