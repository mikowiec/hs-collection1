
module Web.Html where

import Misc hiding (tr)
import Text.Printf

data TagTree
  = TagNode String [(String,String)] [TagTree]
  | Text String
 deriving (Show)

root = TagNode [] [] []
-- tag name cs' Root = TagNode name [] [applyTags cs' Root]
tag name cs' (TagNode n as cs) = TagNode n as (applyTags cs' (TagNode name [] []) : cs)
applyTags [] ys = ys
applyTags (x:xs) ys = x (applyTags xs ys)

-- attr a b Root = Root
attr a b (TagNode n as cs) = TagNode n ((a,b):as) cs

-- txt s Root = Txt s
text s (TagNode n as cs) = TagNode n as (Text s : cs)

html = tag "html"
body = tag "body"
hd = tag "head"
meta = tag "meta"
title = tag "title"
hl = tag "hl"
ul = tag "ul"
li = tag "li"
a  = tag "a"
b  = tag "b"
br = tag "br"

table = tag "table"
tr = tag "tr"
td = tag "td"
th = tag "th"

bold = tag "b"
tt = tag "tt"

script = tag "script"
link = tag "link"
input = tag "input"
div = tag "div"
span = tag "span"

mk_table [] xss = table $ map (tr . map td) xss
-- mk_table hs xss = table $ tr (map th hs) ++ map (tr . map td) xss

quote ('"':xs) = "\\\"" ++ quote xs
quote (x:xs) = x : quote xs
quote [] = []


html_4_0_transitional = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">"
html_4_01_strict = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"


type Markup = [TagTree -> TagTree]

toTree html =
  case (applyTags html (TagNode [] [] [])) of
   TagNode [] [] cts -> cts

newtype CompactShow = CompactShow Markup

instance Show CompactShow where
    showsPrec _ (CompactShow xs) = compMap show_elt (toTree xs)

newtype TreeShow = TreeShow Markup

instance Show TreeShow where
    showsPrec _ (TreeShow xs) = compMap show_elt_tree (toTree xs) . showString "\n"

show_elt e = case fmt_elt e of
              Left s -> showString s
              Right (o,cts,c) -> showString o . compMap show_elt cts . showString c

fmt_elt (Text s) = Left s
fmt_elt (TagNode tag as []) = Right (printf "<%s />" (tagWithAttrs tag as), [], "")
fmt_elt (TagNode tag as cs) = Right (o,cs,c)
  where o = printf "<%s>" (tagWithAttrs tag as)
        c = printf "</%s>" tag
tagWithAttrs tag as = tag ++ concatMap f as
  where f (a,v) = printf " %s=\"%s\"" a v

show_elt_tree e = f 0 e
 where f n e = case fmt_elt e of
                Left s -> showString s
                Right (o,[],"") -> showString ('\n' : replicate n ' ' ++ o)
                Right (o,cts,c) -> showString ('\n' : replicate n ' ' ++ o) .
                                   compMap (f (n+1)) cts .
                                   showString (g cts c)
                 where g (Text _:_) c = c
                       g _ c = ('\n' : replicate n ' ' ++ c)


show_compact_html htmlVersion p = show $ CompactShow $ text htmlVersion : p
show_html htmlVersion p = show $ TreeShow $ text htmlVersion : p

header t cts = hd (title [text t] : cts)

--enc = make "http-equiv" "Content-type"
enc = meta [attr "http-equiv" "Content-type",
            attr "content" "text/html; charset=iso=885901"]

-- page t = html (header t enc)

