
module Net.Sendmail where


import Text.Printf
import Text.Regex
import Network.BSD
import Network
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Network.Socket


import Prelude hiding (catch)
import IO hiding (catch)
import Control.Concurrent
import Control.Exception

import Net.Talk
import Misc


type Header = (String, String)
type Headers = [Header]

data MailConf = MailConf {
    mailsrv  :: Maybe String,
    env_from :: Maybe String,
    env_to   :: Maybe String,
    srv_port :: Maybe Int,
    headers  :: Headers
  }

init_conf = MailConf Nothing Nothing Nothing Nothing []


addr_rx = mkRegex ".*<(.*)>.*|([^<]*)"
get_addr s = case matchRegex addr_rx s of
              Just [a,""] -> Just a
              Just ["",a] -> Just a
              _ -> Nothing

port     p mc = mc { srv_port = Just p }
server   s mc = mc { mailsrv = Just s }
from     a mc = case get_addr a of
                 Just a' -> mc { env_from = Just a', headers = ("From", a) : headers mc }
                 Nothing -> mc
to       a mc = case get_addr a of
                 Just a' -> mc { env_to = Just a', headers = ("To", a) : headers mc }
                 Nothing -> mc
subject  s mc = header "Subject" s mc
header l v mc = mc { headers = (l,v) : headers mc }


sendmail cs body = do
 let conf = foldr ($) init_conf cs
 srv <- case mailsrv conf of
         Just s -> return s
         Nothing -> (getHostByName "mail" >> return "mail")
                      `catch` (\_-> return "localhost")
 Just to <- return (env_to conf)
 Just from <- return (env_from conf)
 let hdrs = headers conf
 let p = maybe (Service "smtp") (PortNumber . fromIntegral) (srv_port conf)
 send_mail srv p from to hdrs body


getFQDN = do
    hn <- getHostName
    he <- getHostByName hn
    (fqdn:_) <- return $ filter ('.' `elem`) (hostName he : hostAliases he)
    return fqdn

with_conn s p f = do
    h <- connectTo s p
    f h `finally` (hClose h)

send_mail server port from to hdrs body = do
 me <- getFQDN
 with_conn server port $ \h -> do
    resp <- expect h 220
    putNet h $ "HELO " ++ me
    greet <- expect h 250
    putNet h $ "Mail from: " ++ from
    send_ok <- expect h 250
    putNet h $ "Rcpt to: " ++ to
    rcpt_ok <- expect h 250
    putNet h "DATA"
    enter_mail <- expect h 354
    mapM_ (\(l,v) -> putNet h (l ++ ": " ++ v)) hdrs
    putNet h ""
    putNet h body
    putNet h "."
    accepted <- expect h 250
    putNet h "quit"
    return ()

{-
main :: IO ()
main = do
    sendmail [server "mail.set.zarquon.se", port 2525, from "test@zarquon.se",
              to "Peter Strand <peter@set.zarquon.se>", subject "Sendmail.hs test",
              header "X-Haskell" "Yar!"]
             "yay!"
    return ()

-}


