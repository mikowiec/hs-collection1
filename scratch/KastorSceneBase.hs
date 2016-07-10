
module KastorSceneBase where

import Data.Int
import Data.Word
import Data.Bits
import Data.Typeable
import Char
import FixedPoint
import KTypes
-- import Language.Haskell.TH
-- import KastorSceneTH
-- import KastorSceneInst

data AttrString = MkAttrString String
 deriving Show
data AttrColor = MkAttrColor Word32
 deriving Show
data AttrTimeMS = MkAttrTimeMS Int32
 deriving Show

class MkAttr a where
 mkAttr :: String -> Attr n a

instance MkAttr Int16x16 where
 mkAttr s = Attr s AttrX

instance MkAttr [Int16x16] where
 mkAttr s = Attr s AttrXList

instance MkAttr AttrString where
 mkAttr s = Attr s (\(MkAttrString s) -> AttrString s)

instance MkAttr AttrColor where
 mkAttr s = Attr s (\(MkAttrColor c) -> AttrColor c)

instance MkAttr KID where
 mkAttr s = Attr s AttrKID

instance MkAttr AttrTimeMS where
 mkAttr s = Attr s (\(MkAttrTimeMS c) -> AttrTimeMS c)

data Binding = Binding (Maybe Int) String AttrType String
 deriving (Typeable)
instance Show Binding where
 show (Binding id n t v) = "bind " ++ show id ++ ":" ++ n ++ ":" ++ v ++ " :: " ++ show t

data Node = Node String [(String, Attribute)] [Node] [Binding]
 deriving (Show, Typeable)

data NodeT a = NodeT Node
 deriving Show

data Attr n a = Attr String (a -> Attribute)

addAttr :: NodeT n -> Attr n a -> a -> NodeT n
addAttr (NodeT (Node n as cs bs)) (Attr l f) v = NodeT (Node n ((l,f v):as) cs bs)

node :: NodeT n -> [NodeT n -> NodeT n] -> NodeT n' -> NodeT n'
node n xs (NodeT (Node p as cs bs)) = NodeT (Node p as (n':cs) bs)
 where (NodeT n') = foldr (\a b -> a b) n xs

root :: (NodeT n -> NodeT n) -> Node
root f = case f (NodeT (Node "" [] [] [])) of
  (NodeT (Node _ _ [c] _)) -> c
  n -> error (show n)

color ('#':r:g:b:[]) = MkAttrColor (0xff000000 .|. (c2i r `shiftL` 28) .|. (c2i g `shiftL` 20) .|. (c2i b `shiftL` 4))
color ('#':r1:r2:g1:g2:b1:b2:[]) = MkAttrColor (0xff000000 .|. ((c2i r1*16+c2i r2) `shiftL` 24)  .|. ((c2i g1*16+c2i g2) `shiftL` 16)  .|. ((c2i b1*16+c2i b2)))
color "red" = MkAttrColor 0xffff0000
color "green" = MkAttrColor 0xff00ff00
color "blue" = MkAttrColor 0xff0000ff
color "black" = MkAttrColor 0xff000000
color "white" = MkAttrColor 0xffffffff

c2i c | c >= '0' && c <= '9' = fromIntegral $ ord c - ord '0'
c2i c | c >= 'A' && c <= 'F' = fromIntegral $ 10 + (ord c - ord 'A')
c2i c | c >= 'a' && c <= 'f' = fromIntegral $ 10 + (ord c - ord 'a')


