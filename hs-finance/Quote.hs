
module Quote where

import IO
import List
import Char
import Monad

import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import qualified Text.ParserCombinators.ReadP as R

import Text.Printf

import Misc

import Price

data Quote = Quote {
  q_date :: (Int,Int,Int),
  q_open :: Price,
  q_high :: Price,
  q_low :: Price,
  q_close :: Price,
  q_volume :: Integer,
  q_adj_close :: Price
 } deriving (Eq,Ord,Show)

parse_int :: (Read i, Integral i) => P.Parser i
parse_int = read `liftM` P.many1 P.digit
parse_date = do
  y <- parse_int; P.char '-'
  m <- parse_int; P.char '-'
  d <- parse_int
  return (y,m,d)
parse_price = do
  h <- parse_int
  P.char '.'
  l <- parse_int
  return (Price h l)

parse_quote = do
  d <- parse_date; P.char ','
  o <- parse_price; P.char ','
  h <- parse_price; P.char ','
  l <- parse_price; P.char ','
  c <- parse_price; P.char ','
  v <- parse_int; P.char ','
  ac <- parse_price
  return (Quote d o h l c v ac)
  

parsePrice :: String -> Quote
parsePrice s =
  either (error . show) id (P.parse parse_quote "" s)

printQuote (Quote (y,m,d) o h l c v ac) =
  printf "%d-%02d-%02d,%s,%s,%s,%s,%d,%s" y m d (show o) (show h) (show l) (show c) v (show ac)

parseDateR = do
  y <- parseIntR; R.char '-'
  m <- parseIntR; R.char '-'
  d <- parseIntR
  return (y,m,d)

parsePriceR = do h <- parseIntR; R.char '.'; l <- parseIntR; return (Price h l)

parseIntR :: (Read i, Integral i) => R.ReadP i
parseIntR = read `liftM` R.many1 (R.satisfy isDigit)

parseQuoteR = do
  d <- parseDateR; R.char ','
  o <- parsePriceR; R.char ','
  h <- parsePriceR; R.char ','
  l <- parsePriceR; R.char ','
  c <- parsePriceR; R.char ','
  v <- parseIntR; R.char ','
  ac <- parsePriceR
  return (Quote d o h l c v ac)

parseQuote :: String -> Quote
parseQuote s = case R.readP_to_S parseQuoteR s of
                 ((q,_):_) -> q
                 _ -> error (printf "Failed to parse \"%s\"" s)
  
readQuoteFile :: FilePath -> IO [Quote]
readQuoteFile fn = do
  cts <- readFile fn
  return (map parseQuote (dropWhile ("Date" `isPrefixOf`) (lines cts)))


readQuoteFileForSymbol sym = do
  readQuoteFile ("quotes/"++ tr ' ' '_' sym)
  

main = do
  qs <- readQuoteFileForSymbol "NOKI SDB"
  print (length qs)
  let avg q = (priceToFloat (q_close q) + priceToFloat (q_high q) + priceToFloat (q_low q) + priceToFloat (q_open q)) / 4
  print (sum (map (avg) qs))


