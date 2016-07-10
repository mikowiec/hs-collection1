
module Net.Http where

import Network.BSD
import Network.Socket

import Char

import Text.Regex
import Text.Printf

import System.Posix.Signals
import System.IO

import Test.HUnit

url_rx = mkRegex "(http://)?([^:/]+)(:([0-9]+))?(/.*)?"

parseUrl :: String -> Maybe (String,Int,String)
parseUrl url = case matchRegex url_rx url of
  Just [_,host,_,"",query] -> Just (host,80,fixQ query)
  Just [_,host,_,port,query] -> Just (host,read port,fixQ query)
  _ -> Nothing
 where fixQ "" = "/"
       fixQ q = q

tests = test $ map (uncurry url_test) $ 
  ("zarquon.se",           ("zarquon.se",80,"")) :
  ("zarquon.se/",           ("zarquon.se",80,"/")) :
  ("http://zarquon.se",    ("zarquon.se",80,"")) :
  ("http://zarquon.se:80", ("zarquon.se",80,"")) :
  ("http://zarquon.se:80/", ("zarquon.se",80,"/")) :
  []
url_test url result = (parseUrl url == Just result) ~? (printf "%s not parsed as %s" url (show result))
doTest = runTestTT tests

readHeaders h = do
  l <- hGetLine h
  case break (==':') l of
   ("","") -> return []
   ("\r","") -> return []
   (l,':':v) -> do
    hs <- readHeaders h
    return ((l,dropWhile isSpace v) : hs)

resp_rx = mkRegex "HTTP/[0-9]\\.[0-9] ([0-9]+) ([^\\r]+)\\r?"

fetchSimplePage :: String -> IO (Int, String, [(String,String)], String)
fetchSimplePage url = withSocketsDo $ do
  installHandler sigPIPE Ignore Nothing
  Just (host,port,query) <- return $ parseUrl url
  ha <- getHostByName host
  (addr:_) <- return $ hostAddresses ha
  s <- socket AF_INET Stream 0
  connect s (SockAddrInet ((fromIntegral port)) addr)
  h <- socketToHandle s ReadWriteMode
  hPutStr h (printf "GET %s HTTP/1.0\r\nHost: %s\r\n\r\n" query host)
  hFlush h
  resp <- hGetLine h
  hdrs <- readHeaders h
  cts <- hGetContents h
  case matchRegex resp_rx resp of
   Just [code, msg] -> return (read code, msg, hdrs, cts)
   _ -> fail (printf "Failed to parse response \"%s\"" resp)

printSimplePage url = do
  (resp,msg,hs,cts) <- fetchSimplePage url
  printf "%d %s\n" resp msg
  mapM (uncurry (printf "%s: %s\n")) hs
  putStr "\n"
  putStr cts



