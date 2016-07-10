
module Sys.Shell where

import Prelude hiding (catch)
import IO hiding (catch, bracket, try)
import Control.Concurrent
import Control.Exception

import List
import Monad

-- import Sys.Process
-- import System.Posix hiding (openFd, createPipe)
import System

import DeepSeq
import Misc

import Control.Concurrent

import Sys.Temp

import System.Cmd

import GHC.Handle (openFd)
import GHC.IOBase
import System.Posix.Internals (FDType(..))
import System.Posix.Types (Fd(..))

import System.IO (openBinaryFile)

import System.Process

run :: String -> [String] -> IO ExitCode
--run path args = rawSystem $ unwords $ path : args
run path args = rawSystem path args

{-
create_pipe = do
    (fr,fw) <- createPipe
    hr <- fdToHandle' fr ReadMode
    hw <- fdToHandle' fw WriteMode
    return (hr,hw)
-}


fdToHandle' (Fd fd) m = do
    openFd (fromIntegral fd) (Just Stream) False "<foo>" m False

hGetAll h = reverse $^ f []
 where f acc = do
         b <- hIsEOF h `catch` (\e -> print "bah">>return False)
         if b then return acc
          else do
                c <- hGetChar h
                f (c:acc)

fdPutAll fd@(Fd ifd) s = do
    h <- fdToHandle' fd WriteMode
    hPutStr h s

--run_capture :: String -> [String] -> Maybe String -> IO (String,String)
run_capture path args inp = do
    with_temp_file $ \(f1,h1) ->
     with_temp_file $ \(f2,h2) ->
      with_temp_file $ \(f3,h3) -> do
       h1 <- case inp of
        Nothing -> return Nothing
        Just s -> do
            hPutStr h1 s >> hClose h1
            h1 <- openBinaryFile f1 ReadMode
            return (Just h1)
       ph <- runProcess path args Nothing Nothing h1 (Just h2) (Just h3)
       r <- waitForProcess ph
       maybeM h1 hClose
       hClose h2 >> hClose h3
       ho <- openBinaryFile f2 ReadMode
       he <- openBinaryFile f3 ReadMode
       o <- hGetAll ho
       e <- hGetAll he
       hClose ho
       hClose he
       case r of
        ExitSuccess -> return (Right (o,e))
        ExitFailure e -> return (Left e)

{-
run_capture' :: String -> [String] -> Maybe String -> IO (Either Int (String,String))
run_capture' path args inp = do
    (fdor,fdow) <- createPipe
    (fder,fdew) <- createPipe
    out <- newEmptyMVar
    err <- newEmptyMVar
    inh <- maybeM inp $ \s -> do
        (fdir,fdiw) <- createPipe
        forkIO ((hPutStr fdiw s >> hClose fdiw) `catch` (\e -> print ("closeFd ",e)))
        return fdir
    p1 <- forkIO ((hGetAll fdor >>= \s -> hClose fdor>> putMVar out s)  `catch` (\e -> print ("closeFd ",e) >> putMVar out ""))
    p2 <- forkIO ((hGetAll fder >>= \s -> hClose fder>> putMVar err s)  `catch` (\e -> print ("closeFd ",e) >> putMVar err ""))
    ph <- runProcess path args Nothing inh (Just fdow) (Just fdew)
    s <- waitForProcess ph
    maybeM inh hClose
    hClose fdow `catch` (\e -> print ("closeFd fdow ",fdow,e))
    hClose fdew `catch` (\e -> print ("closeFd fdew ",fdew,e))
    case s of
     ec -> do
        (o,e) <- liftM2 (,) (takeMVar out) (takeMVar err)
        case ec of
         ExitSuccess -> return (Right (o,e))
         ExitFailure n -> return (Left n)

fdGetAll fd@(Fd ifd) = do
    setFdOption fd NonBlockingRead True
    concat $^ fetch []
 where fetch acc = do
            threadWaitRead (fromIntegral ifd)
            (s,n) <- onEof ([],0) $ fdRead fd 500
            if n == 0 then return $ reverse (s:acc)
             else fetch (s:acc)
         `catch` (\e -> fetch acc)

onEof v m = catchJust ioErrors m f
 where f ioe | isEOFError ioe = return v
       f ioe = ioError ioe


test = do
    Right (s,e) <- run_capture "./wait" [] Nothing
    print s

main :: IO ()
main = do
    forkIO test
    forkIO test
    forkIO test
    forkIO test
    getChar
    return ()

-}

