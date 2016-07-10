
module Crypto where


import Char
import Bits
import Numeric
import Codec.Encryption.SHA1Aux
import Codec.Encryption.SHA1 as SHA1


sign key cts = hmac sha1_oc key cts

sha1_oc :: [Char] -> [Char]
sha1_oc = map (chr . fromIntegral) . SHA1.hash . map (fromIntegral . ord)

toHex :: [Char] -> String
toHex xs = (foldr f id xs) ""
 where f h s = showHex' (ord h) . s

showHex' h | h < 16 = showChar '0' . showHex h
showHex' h = showHex h

hmac f key cts = toHex $ h ((k `xors` opad) ++ h ((k `xors` ipad) ++ text))
 where
  h = f
  ki = map ord key
  k = map chr $ take 64 (ki ++ repeat 0)
  ipad = map chr $ take 64 (repeat 0x36)
  opad = map chr $ take 64 (repeat 0x5c)
  text = cts
  as `xors` bs = map (\(a,b) -> chr (ord a `xor` ord b)) (zip as bs)
