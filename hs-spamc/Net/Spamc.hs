
module Net.Spamc where



import Prelude hiding (catch)
import IO hiding (catch, bracket, try)
import Control.Concurrent
import Control.Exception

import List
import Char
import Misc.Pf
import Misc
import Network.Socket
import System
import System.Posix
import Net.Misc
import Net.Mail
import Text.Regex
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Word
import Monad
import Misc.Logger

spamd_port = 783
spamd_host = "127.0.0.1"

data Mode = Process | Report | Check | Symbols

instance Show Mode where
 show Process = "PROCESS"
 show Report  = "REPORT"
 show Check   = "CHECK"
 show Symbols = "SYMBOLS"


spamc :: (?logger :: Logger) => Mode -> [RawData] -> IO String
spamc mode msg =
  bracket (do logmsg "spamc begin"
              socket AF_INET Stream 0)
    (\s -> do try $ sClose s
              logmsg "spamc end") $
  \s -> do
    user <- userName $^ getUserEntryForID =<< getRealUserID
    sh <- inet_addr spamd_host
    connect s (SockAddrInet spamd_port sh)
    let hdr = sf (fmt""&." SPAMC/1.2\r\nUser: "&."\r\n\r\n") (show mode) user
    send_all s hdr
    send_all_arr_max s 64000 msg
--     mapM (send_all_arr s) (crop_message 64000 arrs)
    shutdown s ShutdownSend
    str <- read_all s
    return str

spam_status = mkRegex "^SPAMD/([0-9.]+) ([0-9]+) ([A-Za-z_]+)"
spam_res_rx = mkRegex "^Spam: ([A-Za-z]+) ; ([0-9./ -]+)"

spam_filter :: (?logger :: Logger) => Message -> IO (Either (String, String) String)
spam_filter msg@(Message from to hs body) = do
    resp <- spamc Symbols (rawmsgRD msg)
    _lns@(status:result:verbose:_) <- return $ filter (not.null) $ lines_crlf resp
    do
        Just [_,_,"EX_OK"] <- return $ matchRegex spam_status status
        Just [r,score] <- return $ matchRegex spam_res_rx result
        if map toLower r `elem` ["yes","true"] then return (Left (score,verbose))
         else return (Right score)
     `catch` \e ->
        return (Right $ "0.0 / 0.0 ; Net.Spamc error: \r\n\t" ++
                        show (status,result) ++ "\r\n\t" ++ err_to_hdrs e)
 `catch` \e -> 
    return (Right $ "0.0 / 0.0 ; Net.Spamc error: " ++ err_to_hdrs e)

spam_headers :: Either (String, String) String -> Headers
spam_headers (Right score) = [("Y-Spam-Status", "No, " ++ score)]
spam_headers (Left (score,verbose)) =
    [("Y-Spam-Flag", "YES"),
     ("Y-Spam-Status", "Yes, "++score),
     ("Y-Spam-Tests", concat $ fmt_syms 15 (splitBy ',' verbose))]

fmt_syms :: Int -> [String] -> [String]
fmt_syms _ [] = []
fmt_syms _ [x] = [x]
fmt_syms n (x:y:xs) | m < 60 = x : "," : fmt_syms m (y:xs)
 where m = n + length x
fmt_syms _ (x:xs) = ('\r':'\n':'\t':x) : "," : fmt_syms 8 xs

hGetArrays _ sz | sz <= 0 = return []
hGetArrays h sz = do
    let l = min sz 50000
    a <- newArray (0,l-1) 0
    hGetArray h a l
    as <- hGetArrays h (sz-l)
    return (a:as)

main = with_stderr_log (do
    [a] <- getArgs
    h <- openFile a ReadMode
    sz <- fromIntegral `liftM` hFileSize h
    arrs <- hGetArrays h (sz)
    arrs' <- mapM freeze arrs
    r' <- spam_filter (Message "peter@zarquon.se" (Forward "peter@zarquon.se")
                            [("From","peter"),("Subject","MAKE MONEY FAST!!!$$$")] arrs')
    mapM_ print (spam_headers r'))

