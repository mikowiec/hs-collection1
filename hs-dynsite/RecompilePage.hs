
module RecompilePage where

import Network.CGI
import Text.Html
import Text.Regex
import System.Cmd
import System.Environment (getArgs)
import System.Posix.Env
import System.Exit
import Monad
import Maybe
import List

import Control.Exception
import Prelude hiding (catch)

import Misc.Misc


cgi_make me m = do
    fresh <- getEnv "fresh" `catch` (\_->return Nothing)
    case fresh of
     Just "true" -> m
     Nothing -> do setEnv "fresh" "true" True
                   rc <- system $ "make " ++ me ++ " >"++ me++".err" ++ " 2>&1"
                   case rc of
                    ExitSuccess -> do 
                        system ("./"++me)
                        return ()
                    ExitFailure _ -> do
                        cts <- readFile (me++".err")
                        wrapper $ \_ -> return $ thehtml (body (pre (stringToHtml cts)))


