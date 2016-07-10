
module Net.MilterChannel where

import Control.Concurrent.Chan

import Data.IORef
import Maybe
import System.Posix
import Control.Exception
import Control.Concurrent

import System.IO
import System.Directory

import Network
import Network.BSD

import Net.Milter
import Net.MilterProtocol
import Net.MilterProtocolIO

import Text.Printf

import Misc

type Macros = [(String,String)]

clientLoop h commands responses = do
  input <- try $ readPacket h
  case input of
    Right p -> writeChan commands (Right (packetToCommand p))
    Left _ -> writeChan commands (Left ConnectionAborted)
  output <- readChan responses
  case output of
    Right resp -> writePacket h (responseToPacket resp) >> hFlush h >> clientLoop h commands responses
    Left ConnectionAborted -> hClose h
 
{-
  do
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
   `catch` (\e -> fail ("Error handling packet ["++(take 100 (show p))++"]: " ++ e))
-}

data Control = ConnectionAborted
 deriving Show

data MilterConnection = MilterConnection (Chan (Either Control Command)) (Chan (Either Control Response))

startMilter :: String -> IO (Chan MilterConnection, IO ())
startMilter socket = do
  installHandler sigPIPE Ignore Nothing
  try $ removeFile socket
  sock <- listenOn (UnixSocket socket)
  main <- newChan
  id <- forkIO $ do
    let loop = do
          (h,hn,pn) <- accept sock
          clientCommands <- newChan
          clientResponses <- newChan
          forkIO $ clientLoop h clientCommands clientResponses
          writeChan main (MilterConnection clientCommands clientResponses)
          loop
    loop
  return (main, killThread id >> sClose sock >> try (removeFile socket) >> return ())

