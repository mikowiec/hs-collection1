
module Pf where

import Numeric
import Misc

data Arg fmt a = Arg fmt (a -> String)
data Lit fmt   = Lit fmt String

class Format fmt a k | fmt k -> a where
    fmt :: ([String] -> k) -> fmt -> [String] -> a

instance Format f x k => Format (Arg f a) (a -> x) k where
    fmt k (Arg fm f) acc = \a -> fmt k fm (f a:acc)

instance Format f x k => Format (Lit f) x k where
    fmt k (Lit fm s') acc = fmt k fm (s':acc)

instance Format [Char] a a where
    fmt k xs acc = k $ reverse (xs:acc)


fixup2 :: [String] -> String
fixup2 = concat

fixup3 :: [String] -> String -> String
fixup3 xs = compMap showString xs


fn :: a -> a
fn f = f

flt :: Int -> Float -> String
flt = float_prec

dbl :: Int -> Double -> String
dbl = float_prec

float_prec n = reverse . dropWhile (=='0') . reverse . (\f -> showFFloat (Just n) f "")

int :: Int -> Int -> String
int = int_fill ' '

int_fill :: Char -> Int -> Int -> String
int_fill c n d = 
    let s = show d
        k = n - length s
    in replicate (max k 0) c ++ s

infixr 4 &>, <&, <&>
(&>) :: String -> fmt -> Lit fmt
(<&) :: (a -> String) -> fmt -> Arg fmt a
(&>) s a = Lit a s
(<&) fn a = Arg a fn

(<&>) :: String -> fmt -> Lit fmt
(<&>) = (&>)

infixr 4 &
(&) :: Show a => String -> fmt -> Lit (Arg fmt a)
(&) s a = s &> show <& a

infixr 4 &.
(&.) :: String -> fmt -> Lit (Arg fmt String)
(&.) s a = s &> id <& a

printf :: Format fmt a (IO ()) => fmt -> a
printf f = fmt (putStr.fixup2) f []

sprintf :: Format fmt a String => fmt -> a
sprintf f = fmt fixup2 f []

ssprintf :: Format fmt a (String -> String) => fmt -> a
ssprintf f = fmt fixup3 f []


pt_fmt :: (Float,Float,Float) -> String
pt_fmt (x,y,z) = sprintf ("("&>flt 2<&", "&>flt 2<&", "&>flt 2<&")") x y z

q_fmt :: ((Float,Float,Float), Float) -> String
q_fmt ((a,b,c),s) = sprintf ("("&>flt 2<&", "&>flt 2<&", "&>flt 2<&"  "&>flt 2<&")") a b c s


pv_showvec (x,y,z) = sprintf ("<"&", "&", "&">") x y z

infixr 4 `pv_vec`
pv_vec s a = s &> pv_showvec <& a

infixr 4 `pv_vec2`
pv_vec2 s a = s &> pt_fmt <& a

