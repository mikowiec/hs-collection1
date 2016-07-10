
module OMXNordic where

import qualified Data.ByteString.Char8 as B
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Monad

import Test.HUnit

nordic_list_2007_march = "19909_The_Nordic_list_March_7__2007.csv"

data Stock = Stock {
  stock_name :: String,
  stock_currency :: String,
  stock_market :: String,
  stock_subMarket :: String,
  stock_category :: String,
  stock_shortName :: String,
  stock_isin :: String,
  stock_issuer :: String,
  stock_previousList :: String
 } deriving (Eq,Show,Read)

optional def p = p <|> return def

parse_stock_def = do
  name <- quotedWord;   c; quotedWord; c; cur <- quotedWord;       c
  market <- quotedWord; c; quotedWord; c; subMarket <- quotedWord; c
  cat  <- quotedWord;   c;                short <- quotedWord;     c
  isin <- quotedWord;   c;                issuer <- quotedWord;    c; pl <- quotedWord
  return (Stock name cur market subMarket cat short isin issuer pl)
 where c = P.char ','
quotedWord = optional "" (P.between (P.char '\"') (P.char '\"') (P.many (P.satisfy (/='\"'))))

parse_def_line b = case P.parse parse_stock_def "" (B.unpack b) of Right stock -> stock; Left err -> error (show err)

tests = test [parse_def_line example_1_line == example_1_result ~? "B&O simple parse: " ++ show (parse_def_line example_1_line)]

example_1_line = B.pack "\"Bang & Olufsen  B\",,\"DKK\",\"CSE\",\"*\",\"LARGE\",\"Consumer Discretionary\",\"BO B\",\"DK0010218429\",\"BO\","
example_1_result = Stock "Bang & Olufsen  B" "DKK" "CSE" "LARGE" "Consumer Discretionary" "BO B" "DK0010218429" "BO" ""

readOMXList = do
  cts <- B.readFile nordic_list_2007_march
  return (map parse_def_line (init (drop 6 (B.lines cts))))

stockholmStocks = do
  xs <- readOMXList
  return (filter ((=="STO") . stock_market) xs)



