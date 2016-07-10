
module Misc where

import Monad
import Maybe
import List
import Random
import Char

--import Observe
--import IOExts
--import IOExtras


isLeft (Left _) = True
isLeft _ = False
isRight (Right _) = True
isRight _ = False

fromLeft (Left l) = l
fromRight (Right r) = r

fromEither (Left l) = l
fromEither (Right r) = r

filterLeft = map fromLeft . filter isLeft
filterRight = map fromRight . filter isRight

retr :: Eq a => a -> [(a,b)] -> (Maybe b, [(a,b)])
retr key list = retr' list []
 where
  retr' [] vs = (Nothing, vs)
  retr' ((a,b):xs) vs
   | key == a  = (Just b, vs++xs)
   | otherwise = retr' xs ((a,b):vs)

when_ :: Monad m => Bool -> m a -> m ()
when_ b m = when b (m >> return ())

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = do
    b <- p
    when b m

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c t e = do
    b <- c
    if b then t else e

withM :: Monad m => Maybe a -> (a -> m ()) -> m ()
withM m f = maybe nop f m

anyM :: Monad m => [(m Bool, m a)] -> m a
anyM [] = fail "anyM: no match"
anyM ((c,m):xs) = do b <- c; if b then m else anyM xs

fix :: (a -> a) -> a
fix f = let x = f x in x

fst3 :: (a,b,c) -> a
snd3 :: (a,b,c) -> b
trd3 :: (a,b,c) -> c

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

testsize :: [a] -> ([a] -> b) -> b -> b
testsize [] _ e = e
testsize xs f _ = f xs

maybeHead :: a -> [a] -> a
maybeHead o [] = o
maybeHead _ (x:_) = x

mapfst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapsnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapfst f = map (\(a,b) -> (f a, b))
mapsnd f = map (\(a,b) -> (a, f b))

tpmap f (a,b) = (f a, f b)

mfst f (a,b) = (f a, b)
msnd f (a,b) = (a, f b)


catchMaybe :: IO a -> IO (Maybe a)
catchMaybe m = liftM Just m `catch` (\_ -> return Nothing)

filterJust :: [Maybe a] -> [a]
filterJust = map fromJust . filter isJust

gen_map :: ([a] -> (b, [a])) -> [a] -> [b]
gen_map _ [] = []
gen_map f xs = let (a',b) = f xs in a' : gen_map f b

-- Shouldn't these be in the Prelude?

map2::(a->b->c) -> [a] -> [b] -> [c]
map2 f (x:xs) (y:ys) = (f x y):(map2 f xs ys)
map2 _ _ _ = []

map3::(a->b->c->d) -> [a] -> [b] -> [c] -> [d]
map3 f (x:xs) (y:ys) (z:zs) = (f x y z):(map3 f xs ys zs)
map3 _ _ _ _ = []

map4::(a->b->c->d->e) -> [a] -> [b] -> [c] -> [d] -> [e]
map4 f (x:xs) (y:ys) (z:zs) (w:ws) = (f x y z w):(map4 f xs ys zs ws)
map4 _ _ _ _ _ = []

map5::(a->b->c->d->e->f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
map5 f (x:xs) (y:ys) (z:zs) (w:ws) (q:qs) = (f x y z w q):(map5 f xs ys zs ws qs)
map5 _ _ _ _ _ _ = []

mapf::[(a->b)] -> [a] -> [b]
mapf (f:fs) (x:xs) = (f x):(mapf fs xs)
mapf _ _ = []

mapf5::[(a->b->c->d->e->f)] -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
mapf5 (f:fs) (a:as) (b:bs) (c:cs) (d:ds) (e:es)  = (f a b c d e):(mapf5 fs as bs cs ds es)
mapf5 _ _ _ _ _ _ = []

mapf4::[(a->b->c->d->e)] -> [a] -> [b] -> [c] -> [d] -> [e]
mapf4 (f:fs) (a:as) (b:bs) (c:cs) (d:ds) = (f a b c d):(mapf4 fs as bs cs ds)
mapf4 _ _ _ _ _ = []

mapf3::[(a->b->c->d)] -> [a] -> [b] -> [c] -> [d]
mapf3 (f:fs) (a:as) (b:bs) (c:cs) = (f a b c):(mapf3 fs as bs cs)
mapf3 _ _ _ _ = []

mapf2::[(a->b->c)] -> [a] -> [b] -> [c]
mapf2 (f:fs) (a:as) (b:bs) = (f a b):(mapf2 fs as bs)
mapf2 _ _ _ = []

--dbg a = trace (show a)
dbg _ = id

compMap :: (a -> b -> b) -> [a] -> b -> b
compMap _ [] = id
compMap f (x:xs) = f x . compMap f xs

maybeM Nothing  _ = return Nothing
maybeM (Just a) m = liftM Just $ m a

maybeM_ Nothing _ = return ()
maybeM_ (Just a) m = m a >> return ()

norm_rnd :: RandomGen r => r -> (Float,r)
norm_rnd r = randomR (0,1) r

norm_rnds :: RandomGen r => r -> ([Float], r)
norm_rnds r = let (r1,r2) = split r in (randoms r1, r2)

collectM n m i = repeatM n (m i)

repeatM n m = mapM (const m) [1..n]

mk_ival :: (Ord a, Num a) => a -> a -> a -> [a]
mk_ival start step stop
 | start > stop = [stop]
 | otherwise    =
    start : case mk_ival (start+step) step stop of
             [x,y] -> if x == y then [x] else [x,y]
             xs -> xs


{-
    fills in extra values in a list to make sure the that the
    difference between consecutive values is at most "step"
-}

fill_ival :: (Ord a, Fractional a) => [a] -> a -> [a]
fill_ival [] _ = []
fill_ival [x] _ = [x]
fill_ival (x:y:xs) step
 | y - x > step = let nxt = x + min ((y-x)/2) step
                  in x : fill_ival (nxt:y:xs) step
 | otherwise    = x : fill_ival (y:xs) step


-- Gah, vill använda #, men det går inte tillsammans med -fglasgow-exts
infixl 9 .!

(.!) :: s -> (s -> f) -> f
s .! f = f s

infixr 0 $^
-- Same as $, but lifts (^) the function  ;)
($^) :: Monad m => (a -> b) -> m a -> m b
f $^ m = liftM f m

-- ($^^) :: 

around i f m = do
    s <- i
    r <- m
    f s
    return r


iterate' f i = i : seq i' (iterate' f i')
 where i' = f i


splitBy c xs = g $ foldr f ([],[]) xs
 where f c' (ys,zs) | c == c' = ([],ys:zs)
       f c' (ys,zs) = (c':ys,zs)
       g (a,b) = a:b

splitWith p xs = splitWith' [] p xs

splitWith' acc p [] = [reverse acc]
splitWith' acc p (x:xs) | p x = (reverse acc) : splitWith' [] p xs
splitWith' acc p (x:xs) = splitWith' (x:acc) p xs


untilM _ _ [] = return Nothing
untilM p f (x:xs) = do
    r <- f x
    if p r then return (Just (x,r))
     else untilM p f xs

break_at b s = let (pfx,b',s') = f ("",s) in (pfx, b', s')
 where f (xs,ys) | b `isPrefixOf` ys = (reverse xs, b, drop (length b) ys)
       f (xs, y:ys) = f (y:xs, ys)
       f (xs, []) = (reverse xs, [], [])
       


allJust xs | all isJust xs = Just (map fromJust xs)
allJust _ = Nothing


drop_ws = dropWhile isSpace

words_us = words' . drop_ws
 where
    words' [] = []
    words' xs = w:words' (dropWhile isSpace ws)
     where (w,ws) = break_on_unescaped_space'' xs


break_on_unescaped_space xs =
    case dropWhile f (zip (inits xs) (tails xs)) of
     (_:(hs,ts):_) -> (hs,ts)
     _ -> (xs,"")
 where f (_,(e:c:_)) | e /= '\\' && isSpace c = False
       f _ = True

break_on_unescaped_space''' xs = f [] xs
 where f hs ts@(e:c:_)  | e /= '\\' && isSpace c = (reverse (e:hs), tail ts)
       f hs (t:ts) = f (t:hs) ts
       f hs [] = (reverse hs, [])

break_on_unescaped_space'' xs = f [] xs
 where f hs (t@('\\'):ts) = f (t:hs) ts
       f hs ts@(e:' ':_)  = (reverse (e:hs), tail ts)
       f hs ts@(e:'\n':_)  = (reverse (e:hs), tail ts)
       f hs (t:ts) = f (t:hs) ts
       f hs [] = (reverse hs, [])


break_on_unescaped_space' xs = (map fst h, map fst t)
 where (h,t) = (break f (zip xs ('\0':xs)))
       f (c,l) = isSpace c && l /= '\\'

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

foreachM :: Monad m => [a] -> (a -> m b) -> m [b]
foreachM r m = mapM m r

foreachM_ :: Monad m => [a] -> (a -> m b) -> m ()
foreachM_ r m = mapM_ m r

lookupM :: (Eq a, Monad m) => a -> [(a,b)] -> m b
lookupM l t = do
    Just v <- return (lookup l t)
    return v


split_at :: [Int] -> String -> [String]
split_at [] line = [line]
split_at (c:cs) line = xs : split_at cs ys
 where (xs,ys) = splitAt c line


nop :: Monad m => m ()
nop = return ()

p `isNotPrefixOf` s = not (p `isPrefixOf` s)

withDefault :: a -> Maybe a -> a
withDefault d = maybe d id

splitPath :: String -> (String, String)
splitPath path = (reverse dir,reverse file)
 where (file,dir) = break isPathSep (reverse path)

mkPath :: String -> String -> String
mkPath "" file = file
mkPath dir file = dir ++ "/" ++ file

isPathSep :: Char -> Bool
isPathSep = (`elem` "/\\")

tr a b xs = map f xs
 where
  f c | c == a = b
  f c = c

takeLast n = reverse . take n . reverse

