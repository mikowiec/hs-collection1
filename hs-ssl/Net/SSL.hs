
module Net.SSL where

import Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Concurrent

import qualified Network.Socket as N
import System.IO

data MethodTag = MethodTag
data CTXTag = CTXTag
data SSLTag = SSLTag

type Method = Ptr MethodTag
type CTX = Ptr CTXTag
type SSL = Ptr SSLTag

foreign import ccall "openssl/ssl.h OpenSSL_add_all_algorithms" add_all_algorithms :: IO ()
foreign import ccall "openssl/ssh.h SSL_load_error_strings" load_error_strings :: IO ()
foreign import ccall "openssl/ssl.h SSLv3_client_method" v3_client_method :: IO Method
foreign import ccall "openssl/ssl.h SSL_CTX_new" ctx_new :: Method -> IO CTX
foreign import ccall "openssl/ssl.h SSL_CTX_set_default_verify_paths" set_default_verify_paths :: CTX -> IO ()
foreign import ccall "openssl/ssl.h SSL_new" new :: CTX -> IO SSL

foreign import ccall "openssl/ssl.h SSL_set_fd" set_fd :: SSL -> Int -> IO ()
foreign import ccall "openssl/ssl.h SSL_connect" connect :: SSL -> IO Int
foreign import ccall "openssl/ssl.h SSL_get_error" get_error :: SSL -> Int -> IO Int
foreign import ccall "openssl/ssl.h ERR_print_errors" print_errors_fd :: Int -> IO ()

foreign import ccall "openssl/ssl.h SSL_write" raw_write :: SSL -> Ptr CChar -> Int -> IO Int
foreign import ccall "openssl/ssl.h SSL_read" raw_read :: SSL -> Ptr CChar -> Int -> IO Int

while rs w m = do
    r <- m
    case r of
     _ | r `elem` rs -> w >> while rs w m
     _ -> return r

main = do


    sock <- N.socket N.AF_INET N.Stream 6
    N.setSocketOption sock N.KeepAlive 1
    ha <- N.inet_addr "195.163.138.199"
    N.connect sock (N.SockAddrInet 443 ha)
    let fd = fromIntegral $ N.fdSocket sock
    print fd

    add_all_algorithms
    load_error_strings
    method <- v3_client_method
    ctx <- ctx_new method
    set_default_verify_paths ctx
    ssl <- new ctx

    print (method,ctx,ssl)
    set_fd ssl fd

    let loop = do
         l <- connect ssl
         print ("conn",l)
         r <- get_error ssl l
         case r of
          2 -> threadWaitRead fd >> loop
          3 -> threadWaitWrite fd >> loop
          _ -> return l
     
    l <- loop

    print l
    print_errors_fd 2
    
    withCStringLen "GET / HTTP/1.0\r\n\r\n" $ \(p,n) -> do
     let loop = do
          l <- raw_write ssl p n
          print ("write",l)
          r <- get_error ssl l
          case r of
           2 -> threadWaitRead fd >> loop
           3 -> threadWaitWrite fd >> loop
           _ -> return l
          return 1
     l <- loop
     print l
     print_errors_fd 2
           
    allocaBytes 100 $ \p -> do
     let loop = do
          l <- raw_read ssl p 100
          print ("read",l)
          when (l /= -1) $ do
            s <- peekCStringLen (p,l)
            print s
          r <- get_error ssl l
          case r of
           2 -> threadWaitRead fd >> loop
           3 -> threadWaitWrite fd >> loop
           _ -> return l
     l <- loop
     print l
     print_errors_fd 2
           
    
    
    putStrLn "ssl test"

