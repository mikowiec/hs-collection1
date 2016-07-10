
module Parse where


import Char

import Text.ParserCombinators.Parsec

import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T


mparse lbl p i = case parse p lbl i of
                Right r -> return r
                Left e -> fail (show e)


tk = T.makeTokenParser emptyDef
int = T.integer tk
nat = T.natural tk
float = T.float tk

word = many (satisfy isAlpha)

