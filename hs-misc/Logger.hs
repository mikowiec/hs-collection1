
module Logger where

import IO

type Logger = String -> IO ()

logmsg :: (?logger :: Logger) => String -> IO ()
logmsg msg = ?logger msg


with_logger :: Logger -> ((?logger :: Logger) => IO a) -> IO a
with_logger log m =
    let ?logger = log
    in m

with_pfx :: (?logger :: Logger) => String -> ((?logger :: Logger) => IO a) -> IO a
with_pfx pfx m =
    let log = ?logger in
    let ?logger = \msg -> log ("["++pfx++"] "++msg) in
    m
    

with_stderr_log :: ((?logger :: Logger) => IO a) -> IO a
with_stderr_log m = with_logger (hPutStrLn stderr) m


