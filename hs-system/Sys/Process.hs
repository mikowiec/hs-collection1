
module Sys.Process where

import qualified System.Process

runProcess fp args wd env h1 h2 h3 = do
    ph <- System.Process.runProcess fp args wd env h1 h2 h3
    System.Process.waitForProcess ph

{-
-- import qualified System.Posix as Posix

import System.IO 	( Handle )
import System.Exit	( ExitCode(..) )

import Monad

import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Misc
import IO (stdin,stdout,stderr)
-- import System.Posix (exitImmediately)


newtype ProcessHandle = ProcessHandle Posix.CPid


forkTest i = do
    print ("fork test ",i)
    Posix.forkProcess (print ("Forked!",i)>>exitImmediately ExitSuccess) >>= print


runProcess
  :: FilePath
  -> [String]
  -> Maybe [(String,String)]
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> IO ProcessHandle

runProcess cmd args env mb_stdin mb_stdout mb_stderr = block $ do
   pid <- Posix.forkProcess proc
   return (ProcessHandle pid)
 where
   proc = do
	new_fd 0 mb_stdin
	new_fd 1 mb_stdout
	new_fd 2 mb_stderr
	Posix.executeFile cmd True args env
    `catch` (\e -> exitImmediately (ExitFailure 253))

   new_fd _ Nothing = return ()
   new_fd fd (Just handle) = do
	old_fd <- Posix.handleToFd handle
        Posix.closeFd fd
	Posix.dupTo old_fd fd
	return ()

untilJust m = do
    r <- m `catch` (\e -> print e >> threadDelay 100000 >> untilJust m) -- return Nothing)
    case r of
     Nothing -> threadDelay 100000 >> untilJust m
     Just v -> return $ Just v


waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess (ProcessHandle pid) = do
  mb_stat <- untilJust (Posix.getProcessStatus True{-block-} False{-stopped-} pid)
  case mb_stat of
	Nothing			    -> error "waitForProcess: internal error"
	Just (Posix.Exited code)    -> return code
	Just (Posix.Terminated sig) -> return (ExitFailure (fromIntegral sig))



createPipe :: IO (Handle,Handle)
createPipe
   = do (read,write) <- Posix.createPipe
	rh <- Posix.fdToHandle read
	wh <- Posix.fdToHandle write
	return (rh,wh)



test_rp i = do
    ph <- runProcess "./wait" [] Nothing Nothing Nothing Nothing
    waitForProcess ph
    return ()
 `catch` (\e -> print ("ERROR ",i,e))


main2 :: IO ()
main2 = do
    xs <- mapM (\i -> liftM ((,)i) newEmptyMVar) [1..20]
    mapM (\(i,m) -> forkIO (test_rp i >> putMVar m ())) xs
--    work_loop
    mapM (\(i,m) -> print ("wait for",i) >> takeMVar m >> print ("done ",i)) xs
    return ()


main :: IO ()
main = do
--    installHandler processStatusChanged (Catch $ print "child!") Nothing
--    sigs <- getSignalMask
--    setSignalMask (processStatusChanged `deleteSignal` sigs)
    repeatM 1 $ do
        main2
        putStr "\n\n\n\n\n"
    print "WAIT"
    getChar
    return ()
 `finally` (print "DONE")

-}

