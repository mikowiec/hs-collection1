

module Main where

import Network.BSD
import Network.Socket
import Control.Concurrent
import Control.Exception
import System
import System.IO 
import Prelude hiding (catch)
import Char
import List
import Directory
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as Lazy

import qualified Text.ParserCombinators.ReadP as R

import Text.Regex
import Text.Printf

import System.Posix.Signals
import System.Time
import System.Locale

import Maybe
import Misc
import Monad

import Web.Html hiding (div)

import Profile

import System.Console.GetOpt

import qualified Text.Html as Html

import qualified Moin

current_version = "0.0.1"

main = withSocketsDo $ do
    http_serve

data Runtime = Runtime {
  runtime_log :: Int -> String -> IO (),
  runtime_profiling :: Bool,
  runtime_docroot :: String,
  runtime_client :: (Handle,Socket,HostAddress,PortNumber,ClockTime),
  runtime_shutdown :: IO ()
 }

data Request = Request {
  req_time :: Maybe ClockTime,
  req_line :: Maybe Lazy.ByteString,
  req_uri :: Maybe Lazy.ByteString,
  req_headers :: [(Lazy.ByteString,Lazy.ByteString)]
 } deriving (Show)

data Response = Response {
  resp_code :: Int,
  resp_message :: Lazy.ByteString,
  resp_headers :: [(Lazy.ByteString, Lazy.ByteString)],
  resp_data :: Maybe Lazy.ByteString
 } deriving (Show)

type HttpProcessor = Runtime -> (Request,Response) -> IO (Request,Response)

emptyRequest = Request Nothing Nothing Nothing []
emptyResponse = Response 500 (Lazy.pack "Internal Error") [] Nothing


resp_ok resp = resp { resp_code = 200, resp_message = Lazy.pack "OK" }

-- ip ? ? date req code size ref client virt
-- time [handlers]

runtime_client_ip rt = ha
 where (_,_,ha,_,_) = runtime_client rt

runtime_accept_time rt = ct
 where (_,_,_,_,ct) = runtime_client rt

formatTime ct = do
  ct' <- toCalendarTime ct
  return $ formatCalendarTime defaultTimeLocale "[%Y-%m-%d %H:%M:%s]" ct'

p s = Lazy.pack s

lookupHeader k hs = listToMaybe [ v | (h,v) <- hs, h == k ]
req_client req = lookupHeader (p "Client") (req_headers req)
req_host req = lookupHeader (p "Host") (req_headers req)
req_referrer req = lookupHeader (p "Referrer") (req_headers req)

safeLast [] = Nothing
safeLast xs = Just (last xs)

timeDiffMicros td = (tdSec td * 1000000) + (fromIntegral (tdPicosec td `div` 1000000))

quoteString s = "\"" ++ s ++ "\""

formatLogLine rt (req,resp) = do
  ip <- inet_ntoa (runtime_client_ip rt)
  at <- formatTime (runtime_accept_time rt)
  resp_time <- getClockTime
  return $ 
    unwords [
     ip,
     at,
     quoteString $ maybe "" Lazy.unpack (req_line req),
     show $ resp_code resp,
     maybe "-" (show . Lazy.length) (resp_data resp),
     show $ req_referrer req,
     show $ req_client req,
     show $ req_host req,
     show $ req_headers req,
     maybe "-" (show . timeDiffMicros . diffClockTimes resp_time) (req_time req)
   ]

logRequest rt s@(req,Response resp_code resp_message resp_headers resp_data) = do
  line <- formatLogLine rt s
  logMessage rt 1 line
  foreachM_ resp_headers $ \(l,msg) ->
    logMessage rt 2 (printf "    %s: %s" (Lazy.unpack l) (Lazy.unpack msg))
  maybeM resp_data $ \cts ->
    logMessage rt 2 (printf "    Data: %s" (take 50 (Lazy.unpack cts)))
  
  return s

logMessage :: Runtime -> Int -> String -> IO ()
logMessage rt v msg = do
  runtime_log rt v msg

runtime_handle rt = case runtime_client rt of (h,_,_,_,_) -> h
runtime_socket rt = case runtime_client rt of (_,s,_,_,_) -> s

parseReq = do
  R.string "GET "
  q <- R.munch1 ((/=' '))
  R.char ' '
  ver <- R.munch1 (not . isSpace)
  return (q,ver)

parseHeader = do
  l <- R.munch1 (/=':')
  R.char ':'
  R.munch (isSpace)
  v <- R.munch1 (`notElem` "\r\n")
  R.optional (R.char '\r')
  return (l,v)

server_string :: String
server_string = printf "Server: MiniHttpd/%s\r\n" current_version

errorResponse resp code short str = 
 resp {
   resp_code = code,
   resp_message = Lazy.pack short,
   resp_headers = (Lazy.pack "Content-type", Lazy.pack "text/html") : resp_headers resp,
   resp_data = Just $ Lazy.pack $ show_html html_4_01_strict [html [hd [title [text "Error!"]],
                                    body [tag "pre" [text str]]]]
 }

-- (Lazy.pack "Content-Length",Lazy.pack (show (Lazy.length response)))
okResponse resp headers response = 
 resp {
   resp_code = 200,
   resp_message = Lazy.pack "OK",
   resp_headers = headers ++ resp_headers resp,
   resp_data = Just $ response
 }

mkHandler :: String -> HttpProcessor -> (String,HttpProcessor)
mkHandler label action = (label,action)

parseH l = case R.readP_to_S parseHeader (Lazy.unpack l) of
                   ((h,_):_) -> Just h
                   _ -> Nothing

getRequest = mkHandler "get" $ \rt s@(req,resp) -> do
  cts <- Lazy.hGetContents (runtime_handle rt)
  let (req_line,cts') = Lazy.break (== '\n') cts
  let headers = [ (Lazy.pack l, Lazy.pack v) |
                    Just (l,v) <- takeWhile isJust
                            (map parseH (Lazy.split '\n' (Lazy.tail cts')))  ]
  ct <- getClockTime
  let req' = req { req_time = Just ct }
  case R.readP_to_S parseReq (Lazy.unpack req_line) of
   [((q,ver),_)] -> return (req' { req_line = Just (Lazy.takeWhile (/='\r') req_line), req_uri = Just (Lazy.pack q), req_headers = headers } ,resp)
   _ -> return (req', errorResponse resp 400 "Bad request" ("Failed to parse request: " ++ Lazy.unpack req_line))
  

defaultResponse = mkHandler "default" $ \rt s -> case s of
 (req@(Request { req_uri = Just request}),resp@(Response { resp_data = Nothing } ) ) -> do
  logMessage rt 2 ("Defaulting response for: " ++ Lazy.unpack request)
  return (req, errorResponse resp 500 "Internal error"
                  ("Server configuration error, don't know how to handle: " ++ Lazy.unpack request))
 _ -> return s

sendResponse = mkHandler "send" $ \rt s -> case s of
 (req,resp@(Response { resp_data = Just response } ) ) -> do
  Lazy.hPut (runtime_handle rt) (Lazy.pack $ printf "HTTP/1.0 %d %s\r\n" (resp_code resp) (Lazy.unpack $ resp_message resp))
  let headers = resp_headers resp
  foreachM_ headers $ \(l,v) -> do
    Lazy.hPut (runtime_handle rt) (l `Lazy.append` (Lazy.pack ": ") `Lazy.append` v `Lazy.append` (Lazy.pack "\r\n"))
  Lazy.hPut (runtime_handle rt) (Lazy.pack "\r\n")
  hFlush (runtime_handle rt)
  Lazy.hPut (runtime_handle rt) (response)
  hFlush (runtime_handle rt)
  return (req, resp)

home = mkHandler "home" $ \rt s -> case s of
 (req@(Request { req_uri = Just request}), resp@(Response { resp_data = Nothing } )) | request == Lazy.pack "/" -> do
  return (req, okResponse resp [(Lazy.pack "Content-type", Lazy.pack "text/html")] $ Lazy.pack $ 
    show_html html_4_01_strict
      [html [hd [title [text "Hi and Hello!"]],
             body [text "Nothing to see here, please move along."]]] )
 _ -> return s

wiki_str = Lazy.pack "/wiki"

z_home = "/kunder/z/zarquon.se"
pages_dir = z_home ++ "/wiki/data/pages"
-- pages_dir = "/home/peter/meta/src/hs-mini-httpd/wiki-pages"

fetch_page p = do
    let page_dir = pages_dir ++ "/" ++ p
    current <- takeWhile isDigit $^ readFile (page_dir ++ "/current")
    cts <- readFile (page_dir ++ "/revisions/" ++ current)
    return cts

pagesrc_ok = mkRegex "^/wiki($|/[A-Za-z0-9_\\-]*)$"
pagename_ok = mkRegex "^([A-Za-z0-9_\\-]+)$"

wiki rt (req@(Request { req_uri = Just request}), resp@(Response { resp_data = Nothing } )) = do
  case matchRegex pagesrc_ok (Lazy.unpack request) of
   Just [page] | null page || page == "/" -> do
    cts <- list_resp
    ok_resp (show cts)
   Just [page] -> do
    cts <- Moin.parse_page page $^ fetch_page (page)
    ok_resp (show (Moin.render_page (Moin.Cfg "/wiki") (tail page) cts))
   failed -> return (req,resp)
 where
  ok_resp cts = 
   return (req, okResponse resp [(Lazy.pack "Content-type", Lazy.pack "text/html")] $ Lazy.pack $ cts )
  list_resp = do
      xs <- getDirectoryContents pages_dir
      let ys = sort $ filter (isJust . matchRegex pagename_ok) xs
      return $ Html.thehtml $ Html.header hdr Html.+++ Html.body (f ys)

  hdr = Html.thetitle (Html.stringToHtml "moin")
  bd cts = Html.pre cts
  f xs = Html.simpleTable [] [] (map (\(x) ->
          [(Html.anchor (Html.stringToHtml x)) Html.! [Html.href ("/wiki/"++x)] ] ) xs)
wiki _ s = return s


dotdot = Lazy.pack ".."
subPage_rx = mkRegex ("^(/[A-Z0-9_a-z-]+)/([A-Za-z0-9_/-]+\\.[a-z]+)$")
serveDirectory prefix rt (req@(Request { req_uri = Just request}), resp@(Response { resp_data = Nothing } )) = do
 case matchRegex subPage_rx (Lazy.unpack request) of
  Just [pfx,rel] | pfx == prefix -> do
    cts <- Lazy.readFile (runtime_docroot rt ++ "/" ++ prefix ++ "/" ++ rel)
    return (req, resp_ok resp { resp_data = Just cts })
  _ -> return (req, resp)
serveDirectory _ _ s = return s

process_http rt s [] = return ()
process_http rt s ((l,f):fs) = do
  -- when_ (runtime_profiling rt) $ stamp "process" (20 + 10 - length fs) []
  s' <- try $ f rt s
  case s' of
   Right s'' -> process_http rt s'' fs
   Left e -> do
    logMessage rt 0 (printf "Failure in processing step %s.. [%s]: " (show l) (show e))
    process_http rt s fs

http_main rt s@(req,resp) = do
  logMessage rt 2 (printf "Serving: %s" (show (runtime_client rt)))
  process_http rt s $ 
    getRequest :
    home :
    mkHandler "wiki" wiki :
    mkHandler "images" (serveDirectory "/img") :
    mkHandler "css" (serveDirectory "/rightsidebar") :
    defaultResponse :
    sendResponse :
    mkHandler "log" logRequest :
    []
  shutdown (runtime_socket rt) ShutdownSend

data Flag = Verbose | Version | Profiling | Help | Port String | Docroot String
 deriving (Eq,Show)

options = [
  Option ['v'] ["verbose"] (NoArg Verbose) "verbosity",
  Option ['V'] ["version"] (NoArg Version) "show version number",
  Option ['h'] ["help"]    (NoArg Help)    "show help",
  Option ['p'] ["profiling"] (NoArg Profiling) "profiling",
  Option ['P'] ["port"]    (ReqArg Port "PORT") "port number to listen on",
  Option ['d'] ["docroot"] (ReqArg Docroot "DOCROOT") "document root"
 ]

parseArgs as =
  case getOpt Permute options as of
    (fs,_,[]) | Version `elem` fs -> do printf "MiniHttpd %s\n" current_version
                                        exitWith ExitSuccess
    (fs,_,[]) | Help `notElem` fs ->
        return (length [ () | Verbose <- fs],
                Profiling `elem` fs,
                head [ d | Docroot d <- fs ++ [Docroot "."] ],
                head ([ p' | Port p <- fs, (p',[]) <- reads p ] ++ [8080])
                )
    (fs,_,xs) -> do hPutStr stderr $ usageInfo "Usage:" options
                    foreachM_ xs $ \err -> hPutStr stderr err
                    exitWith (ExitFailure 1)

http_serve = do
  as <- getArgs
  (verbosity,profiling,docroot,port) <- parseArgs as
  installHandler sigPIPE Ignore Nothing
  let docroot' = reverse (dropWhile (=='/') (reverse docroot))
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  bindSocket s (SockAddrInet (fromIntegral port) 0)
  listen s 5
  let shutdown = sClose s
  let loop seqno = do
       (s',SockAddrInet pn' ha') <- accept s
       time <- getClockTime
       h' <- socketToHandle s' ReadWriteMode
       let log v msg = when_ (v <= verbosity) $ printf "%d: %s\n" seqno msg
       let runtime = Runtime log profiling docroot' (h',s',ha',pn',time) shutdown
       -- when_ (runtime_profiling runtime) $ stamp "accept" 10 []
       (forkIO (http_main (runtime) (emptyRequest,emptyResponse)))
       loop (succ seqno)
  loop (1::Int)


