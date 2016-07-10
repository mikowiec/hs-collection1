
module KastorSceneTemplates where

import Language.Haskell.TH
import Char
import KastorSceneBase
import NodeDB

{-
data SVG = SVG
 deriving Show
data Rect = Rect
 deriving Show
svg :: NodeT SVG
svg = NodeT (Node "svg" [] [])

rect :: NodeT Rect
rect = NodeT (Node "rect" [] [])
-}

defineNode name = do
  let name_l = map toLower name
      name_n = mkName name
      name_e = return (LitE (StringL name_l))
  node_val <- [| NodeT (Node $(name_e) [] [] []) |]
  return [ DataD [] (mkName name) [] [NormalC name_n []] [mkName "Show"],
           SigD (mkName name_l) (ConT ''NodeT `AppT` ConT name_n),
           ValD (VarP (mkName name_l)) (NormalB node_val) [] ]

{-
instance HasX SVG
instance HasX Rect

instance HasFill Rect
-}

defineInst name attrs = do
  let attrInstances = map mkInst attrs
      mkInst a = InstanceD [] (AppT (ConT (mkName ("Has"++cap a))) (ConT (mkName name))) []
  runIO $ registerNode name attrs
  return $ map mkInst attrs


{-

class HasX n where
 attr_x :: Attr n Int16x16
 attr_x = Attr "x" AttrX

class HasFill n where
 attr_fill :: Attr n String
 attr_fill = Attr "fill" AttrColor

x :: HasX n => Int16x16 -> NodeT n -> NodeT n
x v = \n -> addAttr n attr_x v
fill :: HasFill n => String -> NodeT n -> NodeT n
fill v = \n -> addAttr n attr_fill v
-}

defineAttr tp name kid_name = do
  let name_n      = mkName name
      has_name_n  = mkName ("Has"++cap name)
      attr_name_n = mkName ("attr_"++name)
      attr_name   = return $ VarE $ attr_name_n
  attr_val' <- [| \v n -> addAttr n $attr_name v |]
  mk_attr' <- [| mkAttr $(return $ LitE $ StringL kid_name) |]
  return [
    ClassD [] has_name_n [n] []
        [SigD attr_name_n ((ConT ''Attr `AppT` VarT n) `AppT` ConT tp),
         ValD (VarP attr_name_n) (NormalB mk_attr') [] ],
    SigD (mkName name)
         (ForallT [n] [ConT has_name_n `AppT` VarT n]
              (fun_t [VarT tp, ConT ''NodeT `AppT` VarT n, ConT ''NodeT `AppT` VarT n]) ),
    ValD (VarP name_n)
         (NormalB attr_val') [] ]

defineAttrs tp xs = do
  ys <- mapM (\name -> defineAttr tp name name) xs
  return (concat ys)

cap (x:xs) = toUpper x : xs

n = mkName "n"

fun_t :: [Type] -> Type
fun_t [x] = x
fun_t (x:xs) = ArrowT `AppT` x `AppT` fun_t xs


