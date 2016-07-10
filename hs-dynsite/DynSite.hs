
module DynSite where


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

import RecompilePage
 

arg_rx = mkRegex "([A-Za-z0-9_]+)=(.*)"

getArgVarDefs args = concat $ map maybeToList $ map f args
 where f a = case matchRegex arg_rx a of
              Just [var,val] -> Just (var,val)
              _ -> Nothing
    
mkQueryString vs = concat $ intersperse "&" (map (\(a,b) -> a++"="++b) vs)

cgi_wrap me m = do
    args <- getArgs
    setEnv "QUERY_STRING" (mkQueryString (getArgVarDefs args)) False
    cgi_make me (wrapper (\args -> (cgi_error_wrap args m)))

cgi_error_wrap args _ | ("debug","1") `elem` args = mk_error_page "Debug" args
cgi_error_wrap args m = m args `catch` (\e -> mk_error_page e args)

mk_error_page e args = return $ thehtml $ header hdr +++ body bd
 where hdr = thetitle (stringToHtml "view")
       bd  = pre (stringToHtml $ show e) +++ simpleTable [] [] (map (\(a,b) -> [stringToHtml a,stringToHtml b]) (filter (not . null . snd) args))


failHtml err = paragraph (stringToHtml err) ! [bgcolor red]

