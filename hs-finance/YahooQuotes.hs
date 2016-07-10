
module YahooQuotes where

import Net.Http

import Text.Printf
import System
import Time

import OMXNordic

import Monad
import Directory


mkDate y m d = CalendarTime y (toEnum (pred m)) d 0 0 0 0 undefined undefined undefined undefined undefined

mkYahooName "ALIV SDB" = "ALIV.ST"
{-
mkYahooName "HUSQ A" = "HUSQ-A.ST"
mkYahooName "ORI SDB" = "ORI-SDB.ST"
mkYahooName "VOST SDB" = "VOST.ST"
mkYahooName "KAUP SEK" = ""
mkYahooName "KINV A"   = ""
mkYahooName "KINV B"   = ""
mkYahooName "NDA SEK"  = ""
mkYahooName "NOKI SDB" = ""
mkYahooName "BOL"      = ""
mkYahooName "LUMI SDB" = ""
mkYahooName "MIC SDB"  = ""
mkYahooName "GANT"     = ""
mkYahooName "MTRO SDB A" = ""
mkYahooName "MTRO SDB B" = ""
mkYahooName "UNIB SDB" = ""
mkYahooName "PAR SEK"  = ""
mkYahooName "TYKS SDB" = ""
mkYahooName "BRIN B"   = ""
mkYahooName "HQ"       = ""
mkYahooName "INVK B"   = ""
mkYahooName "TWW SDB A" = ""
mkYahooName "TWW SDB B" = ""
mkYahooName "BETS B"   = ""
mkYahooName "ZODI B"   = ""
mkYahooName "MSON A"   = ""
mkYahooName "MSON B"   = ""
mkYahooName "SVIT B"   = ""
mkYahooName "BALD B"   = ""
mkYahooName "LUXO SDB" = "LUXO-SDB.ST"
-}
mkYahooName st = map f st ++ ".ST"
 where f ' ' = '-'
       f x = x

fetchQuotes fn date q = do
  let url = mkYahooUrl q (1980,1,1) (ctYear date, fromEnum (ctMonth date) + 1, ctDay date)
  resp <- fetchSimplePage url
  case resp of
   (200,msg,hdrs,cts) -> do
      let (c:cs) = lines cts
      writeFile fn (unlines (c:reverse cs))
   _ -> fail ("Failed to fetch " ++ q)

mkYahooUrl :: String -> (Int,Int,Int) -> (Int,Int,Int) -> String
mkYahooUrl sym (fy,fm,fd) (ty,tm,td) = printf
  "http://ichart.finance.yahoo.com/table.csv?s=%s&d=%d&e=%d&f=%d&g=d&a=%d&b=%d&c=%d&ignore=.csv"
  sym
  (tm-1) (td) (ty)
  (fm-1) (fd) (fy)

tr a b xs = map f xs
 where
  f c | c == a = b
  f c = c

untilSuccess [] = return ()
untilSuccess (f:fs) = f `catch` (\_ -> untilSuccess fs)

fetchAllStockholm = do
  now <- getClockTime
  ct <- toCalendarTime now
  ss <- stockholmStocks
  mapM_ (\s -> do
      let sn = stock_shortName s
      let fn = "quotes/" ++ tr ' ' '_' sn
      let y1 = tr ' ' '-' sn ++ ".ST"
      let y2 = filter (/=' ') sn ++ ".ST"
      let y3 = takeWhile (/=' ') sn ++ ".ST"
      let y4 = case sn of
                "KINV B" -> "KINB.ST"
                "BOL" ->  "BOLI.ST"
                "GANT" -> "33818.ST"
                "HQ" -> "HAGQ.ST"
                "INVK B" -> "INVIK-B.ST"
                "DORO" -> "896.ST"
                "ACAN B" -> "FTECB.ST"
                _ -> ""
      b <- doesFileExist fn
      let b = False
      unless b $ do
        print (fn,y1,y2,y3,y4)
        let errmsg = printf "Failed to fetch data for [yahoo %s %s %s %s] [short %s] [name %s]\n" y1 y2 y3 y4 (stock_shortName s) (stock_name s)
        untilSuccess 
          ((map (fetchQuotes fn ct) [y1,y2,y3,y4]) ++ [errmsg])
    ) ss


