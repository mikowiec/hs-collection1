
module ApproxTree where

import qualified List
import FiniteMap

import QuickCheck
import Pf

import DeepSeq


type ID = Int


{-
    A normal map, but allows fast lookups based on a handle
    which is generated upon insertion, thus we can forget the
    key. The count is just for statistics.
-}
data ATree k e = ATree {
    elems :: FiniteMap ID (Int, e),
    keys  :: FiniteMap k ID
  }

instance (DeepSeq k, DeepSeq e) => DeepSeq (ATree k e) where
    deepSeq (ATree e k) r = e `deepSeq` k `deepSeq` r

instance (Show k, Show e) => Show (ATree k e) where
    show (ATree e k) = ""

emptyATree = ATree emptyFM emptyFM


{-
    The "current" tree is used for lookups and insertions, all updates
    are entered into the "other" tree. To access the updated elements 
    do a "cleanup", which swaps trees. This mechanism makes it possible
    to preserve ID:s instead of constantly allocating new ones. It also
    simplifies handling of removed elements, since they will disappear
    automatically if not updated.
-}
data ApproxTree key inp elem = AT { 
    current :: ATree key elem,
    other   :: ATree key elem,
    quick :: FiniteMap (ID,inp) ID,
    next :: ID
  }

instance (DeepSeq k, DeepSeq e) => DeepSeq (FiniteMap k e) where
    fm `deepSeq` r = deepSeq (fmToList fm) r


instance (DeepSeq k, DeepSeq i, DeepSeq e) => DeepSeq (ApproxTree k i e) where
    AT c o q n `deepSeq` r =
     c `deepSeq` o `deepSeq` q `deepSeq` n `deepSeq` r


instance (Show k, Show i, Show e) => Show (ApproxTree k i e) where
    show (AT (ATree e k) _ _ i) = sprintf ("keys: "&"  elts: "&"  nid:"&"") (fmToList k) (fmToList e) i


bumpcount f et id = addToFM_C (\(c,e) _ -> (f c,e)) et id undefined

empty :: (Ord k, Ord i) => ApproxTree k i e
empty = AT emptyATree emptyATree emptyFM 0


ins at key id elem = ATree (addToFM (elems at) id (1,elem)) (addToFM (keys at) key id)

bump at id = at { elems = bumpcount succ (elems at) id }

insert :: (Show e, Ord k) => ApproxTree k i e -> k -> e -> (ApproxTree k i e, ID)
insert at@(AT cur@(ATree et kt) ot q nid) key elem = 
    case lookupFM kt key of
     Nothing  -> (AT (ins cur key nid elem) ot q (succ nid), nid)
     Just eid -> (AT (bump cur eid) ot q nid, eid)

find :: ApproxTree k i e -> ID -> Maybe e
find at id = fmap snd $ lookupFM (elems (current at)) id

size :: ApproxTree k i e -> Int
size = sizeFM . elems . current


upd_current at f = at { current = f (current at) }

remove at id = remove_list at [id] -- upd_current at (\at -> at { elems = delFromFM (elems at) id })
remove_list at xs = upd_current at (\at -> at { elems = delListFromFM (elems at) xs })

update :: (Show e, Ord k, Ord i) => ApproxTree k i e -> ID -> i -> k -> e -> (ApproxTree k i e, ID)
update at@(AT _ oth@(ATree et kt) q nid) id inp key elem =
  case lookupFM q (id,inp) of
   Just id' -> (at { other = bump oth id'}, id')
   Nothing -> case lookupFM kt key of
     Just id' -> (at { other = bump oth id'}, id')
     Nothing -> let (id',nxt) = maybe (id,nid) (const (nid, succ nid)) (lookupFM et id)
                in (at { other = ins oth key id' elem,
                         quick = addToFM q (id,inp) id',
                         next = nxt}, id')

modify at@(AT cur oth q nid) id f =
    let t = elems cur
        cur' = cur { elems = addToFM_C (\(i,e) _ -> (i,f e)) t id undefined }
    in at { current = cur' }

cleanup :: (Ord k, Ord i) => ApproxTree k i e -> ApproxTree k i e
cleanup (AT cur oth _ nid) = AT oth emptyATree emptyFM nid

sharing :: ApproxTree k i e -> Int
sharing = sum . map (max 0 . pred . fst) . eltsFM . elems . current


-- Tests:
{-

prop_insert_new k e =
    let a = (k,e) :: (Int,Int)
        tree = empty
        (tree',id) = insert tree k e
        sz = size tree'
        e' = find tree' id
    in (Just e == e') && sz == 1

prop_insert_old k =
    let a = k :: Int
        tree = empty
        (tree',id) = insert tree k (k^10)
        (tree'',id') = insert tree k (k^5)
        e' = find tree' id
    in (Just (k^10) == e')

prop_size xs =
    let num_diff = length $ List.nub $ List.sort (xs :: [Int])
        tree = foldr (\k t -> fst (insert t k k)) empty xs
    in num_diff == size tree


prop_sharing xs = 
    let num_diff = length $ List.nub $ List.sort (xs :: [Int])
        tree = foldr (\k t -> fst $ insert t k k) empty xs
    in sharing tree + num_diff == length xs

prop_update x y z = and [sharing tree == 0, find tree id4 == Just z,
                         find tree id3 == (if y == z then Just y else Nothing), size tree == 1]
 where  a = x :: Int
        tree1 = empty
        (tree2,id2) = insert tree1 x x
        (tree3,id3) = update tree2 id2 y y
        (tree4,id4) = update tree3 id3 z z
        tree = cleanup tree4

-}


