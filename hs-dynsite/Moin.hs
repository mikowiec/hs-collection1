
module Moin where

import qualified Text.Html as H
import Text.Html ((+++), (!))
import Text.ParserCombinators.Parsec

import Text.Printf
import Misc.Misc

data Simple
 = IntLink String
 | ExtLink String String
 | Text String
 | Meta String String
 deriving (Eq,Read,Show)

data Moin
  = Para [[Simple]]
  | Table [[[Simple]]]
  | List [[Simple]]
  | Heading Int String
  | Rule Int
 deriving (Eq,Read,Show)

p1 `orElse` p2 = do
 r <- try (p1 >>= return . Just)
 case r of
  Just a -> return a
  Nothing -> p2

moin :: Parser Moin
moin = table <|> heading <|> rule <|> list <|> para

meta :: Parser Simple
meta = try $ do
  string "[["
  t <- many1 alphaNum
  char '('
  cts <- manyTill anyChar (string ")]]")
  return $ Meta t cts

table :: Parser Moin
table = Table $^ many1 row

crlf = optional (char '\r') >> char '\n'

row :: Parser [[Simple]]
row = do
  string "||"
  r <- simple `endBy` string "||"
  crlf
  return r

list :: Parser Moin
list = List $^ many1 listRow

listRow = try $ do
  many (char ' ') -- FIXME
  string "* "
  r <- simple
  crlf
  return r


simple :: Parser [Simple]
simple = many1 (meta <|> link <|> text <|> (Text $^ many1 (char ' ')))

link = (int_link1 <|> ext_link <|> int_link2) <?> "link"

ext_link :: Parser Simple
ext_link = (try $ do
  char '['
  url <- many1 (noneOf " ]")
  desc <- (space >> manyTill anyChar (char ']')) <|> return ""
  return $ ExtLink url desc)
 <?> "ext_link"

int_link1 :: Parser Simple
int_link1 = (try $ do
  string "[\""
  page <- many1 alphaNum
  string "\"]"
  return $ IntLink page)
 <?> "int_link1"

int_link2 :: Parser Simple
int_link2 = (try $ do
  c1 <- upper
  cs1 <- many1 lower
  c2 <- upper
  cs2 <- many letter
  return $ IntLink $ c1 : cs1 ++ [c2] ++ cs2)
 <?> "int_link2"


text :: Parser Simple
text = (Text $^ (noneOf "=|[ \r\n" >>= \c -> many (noneOf " [\r\n") >>= \cs -> return (c:cs)))

heading = do
  h <- many1 (char '=')
  text <- manyTill anyChar (string h)
  crlf
  return $ Heading (length h) text

rule = do
  r <- many1 (char '-')
  crlf
  return $ Rule $ length r

para = do
  p <- (simple `endBy1` crlf) <|> (crlf >> return [])
  return (Para p)

test_parse p t = parse p "test" t

moin_parser = do
  m <- many moin
  eof
  return m

parse_page :: String -> String -> [Moin]
parse_page name p = either err id (parse moin_parser name p)
 where err pe = error (show pe)

stylesheet media ss = 
  H.thelink H.noHtml ! [H.rel "stylesheet", H.thetype "text/css", H.strAttr "media" media, H.href ss]

data Cfg = Cfg { script_name :: String }

render_page :: Cfg -> String -> [Moin] -> H.Html
render_page cfg t moin =
  H.thehtml (H.header
               (H.thetitle (H.stringToHtml t) +++
                stylesheet "all" "/rightsidebar/css/common.css" +++
                stylesheet "screen" "/rightsidebar/css/screen.css" +++
                stylesheet "print" "/rightsidebar/css/print.css" +++
                stylesheet "projection" "/rightsidebar/css/projection.css" )
         +++ H.body (render_moin_div cfg t moin))

div_id id div = H.thediv div ! [H.identifier id]
div_class cls div = H.thediv div ! [H.theclass cls]

render_moin_div cfg page moin =
  div_id "page"
    (div_class "pagetitle" (H.h1 (H.stringToHtml page) ! [H.identifier "title"])
 +++ div_id "content" (H.concatHtml $ map (render_moin cfg) moin))

render_moin cfg (Para []) = H.primHtml "\r\n"
render_moin cfg (Para ps) = H.paragraph $ H.concatHtml $ map (\ws -> map (render_simple cfg) ws) ps
render_moin cfg (Table xs) = H.simpleTable [] [] (map (map (H.concatHtml . map (render_simple cfg))) xs)
render_moin cfg (List xs) =
    H.ulist (H.concatHtml $ map (\x -> H.li (H.concatHtml (map (render_simple cfg) x))) xs)
render_moin cfg (Heading 1 s) = H.h1 (H.stringToHtml s)
render_moin cfg (Heading 2 s) = H.h3 (H.stringToHtml s)
render_moin cfg (Heading 3 s) = H.h3 (H.stringToHtml s)
render_moin cfg (Heading 4 s) = H.h4 (H.stringToHtml s)
render_moin cfg (Heading 5 s) = H.h5 (H.stringToHtml s)
render_moin cfg (Heading 6 s) = H.h6 (H.stringToHtml s)
render_moin cfg (Rule n) = H.hr -- FIXME

render_simple cfg (Text s) = H.stringToHtml s
render_simple cfg (Meta "Raw" s) = H.primHtml s
render_simple cfg (IntLink p) = H.anchor (H.stringToHtml p) ! [H.href (printf "%s/%s" (script_name cfg) p)]
render_simple cfg (ExtLink l "") = H.anchor (H.stringToHtml l) ! [H.href l]
render_simple cfg (ExtLink l d) = H.anchor (H.stringToHtml d) ! [H.href l]


