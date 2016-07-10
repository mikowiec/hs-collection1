

module Net.Mail where

import Misc

import Net.Talk
import Text.Regex

import Char

import Data.PackedString
import Data.Array.Unboxed
import Data.Array.IArray
import Data.Array.IO

import IO

type User = String
type Box = String


type Sender = String

data LocalRecipient = HomeBox User Box | SystemBox User
 deriving (Show,Read)
data Recipient = Local LocalRecipient | Forward String
 deriving (Show,Read)

show_recipient (Local (HomeBox u _)) = u
show_recipient (Local (SystemBox u)) = u
show_recipient (Forward fwd) = fwd


type Header = (String, String)
type Headers = [Header]

type RawMessage = PackedString
data Message = Message {
    msg_sender :: Sender,
    msg_recipient :: Recipient,
    msg_headers :: Headers,
    msg_body :: [RawData]
  } deriving Show


addr_rx = mkRegex ".*<(.*)>.*|([^<]*)"

get_addr :: String -> Maybe String
get_addr s = case matchRegex addr_rx s of
              Just [a,""] -> Just a
              Just ["",a] -> Just a
              _ -> Nothing



rawmsg_lf (Message _ _ hs body) = 
    let hdrs = concatMap show_header_lf hs
    in hdrs ++ "\n" ++ concatMap (map (chr . fromIntegral) . elems) body

rawmsg :: Message -> String
rawmsg (Message _ _ hs body) = 
    let hdrs = concatMap show_header hs
    in hdrs ++ "\r\n" ++ concatMap (map (chr . fromIntegral) . elems) body

rawmsgRD :: Message -> [RawData]
rawmsgRD (Message _ _ hs body) =
    let hdrs = map (fromIntegral.ord) $ concatMap show_header hs ++ "\r\n"
        len = length hdrs
    in listArray (0, (len-1)) hdrs : body

hPutUArray h a n = do
    a' <- unsafeThaw a
    hPutArray h a' n

hPutMsg h (Message _ _ hs body) = do
    mapM_ (hPutStr h . show_header) hs
    hPutStr h "\r\n"
    mapM_ (\a -> hPutUArray h a (n a)) body
 where n a = {-# SCC "hPutArray_bounds" #-} 1 + snd (bounds a)

indent = map ("\t"++)

indent' [] = []
indent' [x] = [x]
indent' (x:xs) = x : indent xs

err_to_hdrs :: Show s => s -> String
err_to_hdrs = 
    unlines_crlf_hdr . indent . filter (not.null) . lines . show


parse_from_line s = get_from s

-- Try to get "From foo@sender"..
get_from ('F':'r':'o':'m':' ':xs) = Just (takeWhile (not.isSpace) xs)
get_from _ = Nothing

parse_mail sender recip msg = Message snd recip headers [str_to_arr body]
 where
    snd = maybe sender id (parse_from_line msg)
    (hs, body) = hdr_lines msg
    headers = map parse_header hs


