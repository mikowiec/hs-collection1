
module Net.Talk where

import Network.Socket
import Misc
import IO

import Char
import Monad

import Data.Array.IO
import Data.Array.Unboxed
import Data.Word

type RawData = UArray Int Word8

send_all _ [] = return ()
send_all s msg = do
    r <- send s msg
    send_all s (drop r msg)

send_all_arr_max _ _ [] = return ()
send_all_arr_max _ max _ | max <= 0 = return ()
send_all_arr_max s max (a:as) = do
    let len = snd (bounds a)
        n = min len max
    -- hPutArray h a n
    send_all s (map (chr . fromIntegral) $ elems a)
    send_all_arr_max s (max - n) as

chunk_do _ [] = return ()
chunk_do f xs = do
    let (ys,zs) = splitAt 4000 xs
    f ys
    chunk_do f zs

read_all s = f []
 where f acc = do
        s <- try $ recv s 4000
        case s of
         Right xs -> f (xs:acc)
         Left e | isEOFError e -> return $ concat $ reverse $ acc
         Left e -> ioError e

do_while_ok m = do
    r <- try m
    case r of
     Right s -> do (ss,e) <- do_while_ok m
                   return (s:ss,e)
     Left e | isEOFError e -> return ([],Nothing)
     Left e -> return ([], Just e)


talk s msg = do
    hPutStr s (msg++"\r\n")
    hFlush s
    hGetLine s


str_to_arr :: String -> UArray Int Word8
str_to_arr s = 
    let n = length s
    in listArray (0,n-1) (map (fromIntegral . ord) s)


hdr_lines xs = (merge_cont hs, body)
 where (hs, body) = hdr_lines' [] [] xs
hdr_lines' [] ([]:acc) []   = (reverse acc, [])
hdr_lines' [] acc []        = (reverse acc, [])
hdr_lines' la acc []        = (reverse ((reverse la):acc), [])
hdr_lines' [] ([]:acc) xs   = (reverse acc, xs)
hdr_lines' la acc ('\r':'\n':xs) = hdr_lines' [] ((reverse la):acc) xs
hdr_lines' la acc ('\n':xs) = hdr_lines' [] ((reverse la) : acc) xs
hdr_lines' la acc ('\r':xs) = hdr_lines' [] ((reverse la) : acc) xs
hdr_lines' la acc (x:xs)    = hdr_lines' (x:la) acc xs

merge_cont [] = []
merge_cont (xs:ys@(c:_):hss) | isSpace c = merge_cont ((xs ++ "\r\n" ++ ys) : hss)
merge_cont (xs:hss) = xs : merge_cont hss

parse_header hs = (l,v)
 where (l,_,v) = break_at ": " hs

parse_headers = map parse_header . fst . hdr_lines

show_header (l,v) = l ++ ": " ++ v ++ "\r\n"
show_header_lf (l,v) = l ++ ": " ++ v ++ "\n"

lines_crlf = lines . no_crlf

no_crlf [] = []
no_crlf ('\r':'\n':xs) = no_crlf ('\n':xs)
no_crlf (x:xs) = x : no_crlf xs


unlines_crlf_hdr [] = []
unlines_crlf_hdr [x] = x
unlines_crlf_hdr (x:xs) = x ++ "\r\n" ++ unlines_crlf_hdr xs


expect h n = do
    s <- hGetLine h
    n' <- readIO (takeWhile isDigit s)
    when (n /= n') $ fail ("expected " ++ show n ++ ", got: " ++ s)
    return s

putNet h msg = hPutStr h (msg++"\r\n") >> hFlush h

