
module BorsenDkQuotes where

import Misc

import Maybe
import Char
import List
import Time
import Net.Http
import Text.Printf
import Text.Regex
import Data.Html.TagSoup
import Control.Monad
import Control.Exception

import Text.XML.HaXml as XML hiding (find)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Html.Parse
import Text.XML.HaXml.Html.Pretty as XmlPretty

import Quote
import OMXNordic
import Price


mkUrl :: Int -> (Int,Int,Int) -> (Int,Int,Int) -> String
mkUrl id (s_y,s_m,s_d) (e_y,e_m,e_d) =
  printf "http://trader.borsen.dk/borsendk/site/miniweb/stockpop.page?magic=%%28cc+%%28tsid+%d%%29+%%28view+4%%29%%29&startDate=%d-%d-%d&endDate=%d-%d-%d&x=47&y=7"
         id s_y s_m s_d e_y e_m e_d

{-
 http://trader.borsen.dk/borsendk/site/miniweb/miniweb.page?magic=(cc%20(level1%202)%20(level3%202))
  <option value="(cc (level1 2) (level2 1) (level3 1))">Stockholm</option> 
  <option value="(cc (level1 2) (level2 2) (level3 1))">Helsingfors</option>
  <option value="(cc (level1 2) (level2 3) (level3 1))">Oslo</option>

  <option value="(cc (level1 2) (level3 1))">Large Cap</option>
  <option value="(cc (level1 2) (level3 2))">Mid Cap</option>
  <option value="(cc (level1 2) (level3 3))">Small Cap</option>
  <option value="(cc (level1 2) (level3 4))">NGM OTC</option>
  <option value="(cc (level1 2) (level3 5))">NGM Equity</option>
  <option value="(cc (level1 2) (level3 6))">First North</option>
-}

parseListPage :: String -> [(String,Int)]
parseListPage cts = 
  [ (t,read id) | (t,Just [id]) <- mapsnd (matchRegex magicRx) stocks ]
 where
  tags = parseTagsNoPos cts
  links = map head (sections (~== TagOpen "a" []) tags)
  href_titles = map (\l -> (fromAttrib "title" l, fromAttrib "href" l)) links
  stocks = (filter (("/borsendk/site/miniweb/stockpop.page?magic=" `isPrefixOf`) . snd) href_titles)

mkListUrl :: Int -> Int -> Int -> String
mkListUrl a b c = 
  printf "http://trader.borsen.dk/borsendk/site/miniweb/miniweb.page?magic=(cc%%20(level1%%20%d)%%20(level2%%20%d)%%20(level3%%20%d))" a b c

mkInfoUrl :: Int -> String
mkInfoUrl a = 
  printf "http://trader.borsen.dk/borsendk/site/miniweb/stockpop.page?magic=(cc%%20(tsid%%20%d)%%20(view%%205))" a

fetchExtraInfo (name,id) = do
  (200,_,_,page) <- fetchSimplePage (mkInfoUrl id)
  let tags = parseTagsNoPos page
  let info = takeWhile (~/= TagClose "table") (dropWhile (~/= TagOpen "table" [("class","ruled")]) tags)
  let infos = filter (not . null) (map (map innerText . partitions (~== TagOpen "td" [])) (partitions (~== TagOpen "tr" []) info))
  let labels = ["ISIN", "B\248rs", "Segment", "Kode", "Navn", "M\248ntfod"]
  Just [isin,market,seg,short,name,currency] <- return $ mapM (findInfo infos ) labels
  return (id,isin,market,seg,short,name,currency)

findInfo xs label = takeWhile (not . isSpace) . dropWhile isSpace . last $^ find ((label `isPrefixOf`) . head) xs





fetchLists = do
  (200,_,_,cts) <- fetchSimplePage (mkListUrl 2 1 1)
  let largeCap = parseListPage cts
  (200,_,_,cts) <- fetchSimplePage (mkListUrl 2 1 2)
  let midCap = parseListPage cts
  (200,_,_,cts) <- fetchSimplePage (mkListUrl 2 1 3)
  let smallCap = parseListPage cts
  let lists = (largeCap ++ midCap ++ smallCap)
  extendedLists <- mapM fetchExtraInfo lists
  return extendedLists

cacheLists = do
  xs <- fetchLists
  writeFile "borsen.dk.cache" (show xs)

readCache :: IO [(Int,String,String,String,String,String,String)]
readCache = do
  cts <- readFile "borsen.dk.cache"
  return (read cts)
  
magicRx = mkRegex "magic=\\(cc%20\\(tsid%20([0-9]+)\\)\\)"

fetchQuotePage ccid from to = do
  let url = mkUrl ccid from to
  (200,_,_,resp) <- fetchSimplePage url
  return resp

fetchQuotes ccid from to = do
  page <- fetchQuotePage ccid from to
  return (parseQuotePage page)

parseQuotePage' :: String -> [((Int,Int,Int),Price,Price,Price,Price,Price,Int)]
parseQuotePage' page = [  ]

-- row' :: Content Posn -> [String]
row :: Content Posn -> [String]
row (CElem (Elem "tr" _ cs) _) = map (concatMap content' . r) (filter (isElem "td") cs)

isElem e (CElem (Elem e' _ _) _) = e == e'
isElem _ _ = False

r = txt `o` children `o` tag "td"
content' (CString _ s _) = s

-- parseQuotePage' :: String -> [((Int,Int,Int),Price,Price,Price,Price,Price,Int)]
parseQuotePage :: String -> [((Int,Int,Int),Price,Price,Price,Price,Price,Int)]
parseQuotePage page = [ q | Just q <- res3 ]
 where
  (quotepart:_) = dropWhile ("<table class=\"ruled snipped\">" `isNotPrefixOf`) (tails page)
  XML.Document _ _ (XML.Elem _ _ xmls) _ = htmlParse "dk" quotepart


  
  res1 :: [Content Text.XML.HaXml.Posn.Posn]
  res1 = (tag "tr" `o` children `o` tag "tbody"  $ (xmls!!3))


  res2 :: [ [ String ] ]
  res2 = map row res1
  
  res3 = map f res2

  f (date:close:high:low:ask:bid:volume:_) = parseQuoteLine (date,close,high,low,ask,bid,volume)
  f xs = error (show xs)
  group7 [] = []
  group7 xs = take 7 xs : group7 (drop 7 xs)


  {-
  q3 = map ( fmap parseQuoteLine . f . map innerText) (splitWith (~== TagOpen "tr" []) q1)
  f (_:_:date:_:_:_:close:_:_:_:high:_:_:_:low:_:_:_:ask:_:_:_:bid:_:_:_:volume:_) =
      Just (date,close,high,low,ask,bid,volume)
  f _ = Nothing
-}

parseQuoteLine (d,c,h,l,a,b,v) = do
  [y,m,d] <- matchRegex dateRx d
  close <- matchPrice c
  high <- matchPrice h
  low <- matchPrice l
  ask <- matchPrice a
  bid <- matchPrice b
  vs <- matchRegex volumeRx v
  return ((read y::Int,read m::Int,read d::Int),
          close,
          high,
          low,
          ask,
          bid,
          read (concat vs) :: Int)

matchPrice :: String -> Maybe Price
matchPrice s = do
 [a,b,c] <- matchRegex priceRx s
 return (Price (read a) (10*(read b) + (read c)))

dateRx = mkRegex "([0-9]+)-([0-9]+)-([0-9]+)"
priceRx = mkRegex "([0-9]+),([0-9])([0-9])"
volumeRx = mkRegex "([0-9]+\160?)+"

fetchAllStockholm = do
  now <- getClockTime
  date <- toCalendarTime now
  omx <- readOMXList
  dk <- readCache
  let ss = filter (\s -> stock_market s == "STO") omx
  forM ss $ \s -> try $ do
    let [ccid] = [ id | (id,isin,_,_,_,_,_) <- dk , isin == stock_isin s ]
    printf "Fetch quotes for %s (%d)...\n" (stock_shortName s) ccid
    qs <- fetchQuotes ccid (1980,1,1) (ctYear date, fromEnum (ctMonth date) + 1, ctDay date)
    let fn = printf "quotes/%s.dk" (tr ' ' '_' (stock_shortName s))
    let f ((y,m,d),close,high,low,_,_,volume) = printf "%4d-%02d-%02d,0.00,%s,%s,%s,%d,0.00" y m d (show high) (show low) (show close) volume
    let qs' = map f qs
    writeFile fn (unlines ("Date,_,High,Low,Close,Volume,_" : qs'))
    printf "%s written (%d quotes)\n" fn (length qs)

