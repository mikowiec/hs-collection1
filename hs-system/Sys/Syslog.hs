
module Sys.Syslog where

import Control.Exception
import Foreign.C.Types
import Foreign.C.String
import Data.Bits

log_cons, log_pid :: CInt
log_cons = 2
log_pid = 1
log_mail :: CInt
log_mail = 2 `shiftL` 3

type LogLevel = CInt

log_emerg, log_alert, log_crit, log_err, log_warning, log_notice, log_info, log_debug :: LogLevel
log_emerg = 0
log_alert = 1
log_crit = 2
log_err = 3
log_warning = 4
log_notice = 5
log_info = 6
log_debug = 7

syslog :: LogLevel -> String -> IO ()
syslog level msg = msg `withCString` \msg_p ->
  "%s" `withCString` \fmt -> c_syslog level fmt msg_p

withSysLogging :: String -> CInt -> IO a -> IO a
withSysLogging label facility m = label `withCString` \p -> 
  bracket_ (c_openlog p options facility) (c_closelog) m
 where options = 0

foreign import ccall safe "syslog.h openlog" c_openlog :: CString -> CInt -> CInt -> IO ()
foreign import ccall safe "syslog.h syslog" c_syslog :: CInt -> CString -> CString -> IO ()
foreign import ccall safe "syslog.h closelog" c_closelog :: IO ()

