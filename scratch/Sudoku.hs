
module Sudoku where

import Control.Concurrent

import Data.Word
import Data.IORef
import Data.Array
import Text.Printf

import System
import Char
import IO
import Random
import List
import Maybe

import Misc

import Debug.Trace

import qualified Data.Array.MArray as MArray
import qualified Data.Array.ST as STArray
import Control.Monad.ST
import qualified Data.Set as S

type BoardIndex = Word8
type BoardNum = Word8

type Board = Array (BoardIndex,BoardIndex) BoardNum
type BoardPos = (BoardIndex, BoardIndex)
type BoardMove = (BoardPos,BoardNum)
type BoardMoves = [BoardMove]

safeTail [] = []
safeTail xs = tail xs

generators :: RandomGen g => g -> [g]
generators g = map fst (iterate (split . snd) (split g))


scramble :: RandomGen g => g -> [a] -> [a]
scramble g xs = scramble' g (length xs) xs

scramble' _ _ [] = []
scramble' g n xs = z : scramble' g' (n-1) (ys++zs)
 where (ys,z:zs) = splitAt i xs
       (i,g') = randomR (0,n-1) g

is_valid_set xs = is_valid_set' (filter (/=0) xs)
is_valid_set' [] = True
is_valid_set' xs = all isSingleton (group (sort xs))


isSingleton [x] = True
isSingleton _ = False

check board range = is_valid_set (map (board!) range)

is_valid board = all (check board) subranges
subranges = rows ++ cols ++ subs

rows = [ [ (i,j) | i <- [1..9] ] | j <- [1..9] ]
cols = transpose rows
subs = [ [(k,l) | k <- [i..i+2], l <- [j..j+2] ] | i <- [1,4,7], j <- [1,4,7] ]

positions = [(i,j) | i <- [1..9], j <- [1..9] ]

prefill g n =
 let prefilled_ps = take n (scramble g1 positions)
     (g1,g2) = split g
     (ms,_) = gen_boards' n []
     ms' = dropWhile (null . solve . fst) ms
  in case ms' of
      [] -> prefill g2 n
      ((x,n'):_) -> (x,n')

prettyBoard :: Board -> String
prettyBoard array = 
 unlines $ map prettyRow (map (map (array!)) rows)

prettyBoardIO b = putStr (prettyBoard b)

prettyRow ns = concat $ intersperse " " (map show ns)

empty_board = (array ((1,1), (9,9)) [((x,y),0) | x <- [1..9], y <- [1..9] ])

generate_board g level = do
  let (ms,n) = prefill g (5*level)
  return ms



corner n = succ ((n-1) - (n-1) `mod` 3)

{-
gen_boards nn n _ ms board [] = ([(ms,n)],n)
gen_boards nn n limit ms board (p:ps) | board!p /= 0 = gen_boards nn n limit ms board ps
gen_boards nn n (Just limit) ms board (p:ps) | n >= limit = ([],n)
gen_boards nn n limit ms board (p:ps) = 
  let moves = [ (p,n) | n <- [nn..9] ++ [1..nn-1], mod_is_ok board (p,n) ]
      nxt n | n == 9 = 1
      nxt n = n+1
      (rs,n') = foldr (\m (rs,n') -> 
                        let (rs',n'') = gen_boards (nxt nn) (n'+1) limit (m:ms) (board//[m]) ps
                        in (rs++rs',n'') ) ([],n) moves
  in (rs,n')
-}

mod_is_ok :: Board -> ((BoardIndex,BoardIndex),BoardNum) -> Bool
mod_is_ok board ((i,j),n) =
        let row = [(k,j) | k <- [1..9] ]
            col = [(i,k) | k <- [1..9] ]
            i' = corner i
            j' = corner j
            sub = [(k,l) | k <- [i'..i'+2], l <- [j'..j'+2] ]
            check indices = not (n `elem` (map (board!) indices))
         in all check [row,col,sub]
            -- check (row ++ col ++ sub)


gen_boards' n ms = (map (\ms->(ms,0)) mss, 0)
 where mss = gen_boards'' 0 n (makeMoves ms emptyState) []

gen_boards'' m 0 st ms = [ms]
gen_boards'' m n st@(rca,rda,cda,bda) ms =
 let ms' = [ (makeMove m st, ((r,c),d)) | m@(r,c,d) <- validMoves st ]
     ms'' = [ (st',m) | (Just st',m) <- ms' ]
     newly_covered = length (groupByProj (fst . snd) ms'')
 in if m + newly_covered == 81 then concatMap (\(st,mv) -> gen_boards'' (m+1) (n-1) st (mv:ms)) ms''
      else []

groupByProj p = groupBy (\a b -> p a == p b)


gen_boards''' sel l limit m 0 st ms = (l,[reverse ms])
gen_boards''' sel l limit m n st@(rca,rda,cda,bda) ms | l >= limit = (l,[])
gen_boards''' sel l limit m n st@(rca,rda,cda,bda) ms =
 let ms'' = [ (makeMove' m st, ((r,c),d)) | m@(r,c,d) <- f (validMoves st) ]
     (f:sel') = sel
     g (_,(p,_)) (_,(q,_)) = compare p q
     newly_covered = length (groupByProj (fst . snd) (sortBy g ms''))
 in if m + newly_covered == 81 then
        let (l',rs) = foldr (\(st,mv) (l,rs) ->
                          let (l'',rs') = gen_boards''' sel' (l+1) limit (m+1) (n-1) st ((mv,map snd ms''):ms)
                          in (l'',rs')) (l,[]) ms''
        in (l',rs)
      else (l,[])

prefill_ g n =
  gen_boards''' (scrambles g) 0 1000 0 n emptyState []

selects g = sel : selects g2
 where (sel,g2) = select g

scrambles g = scramble g1 : scrambles g2
 where (g1,g2) = split g

select g = (\xs -> [xs!!(fst (randomR (0,length xs-1) g1))],g2)
 where (g1,g2) = split g

-- solve_from_state n ms = gen_boards''' (repeat id) (length ms) n (makeMoves ms emptyState) []


type Move = (Word8, Word8, Word8)

-- r,c | r,d | c,d | b,d
type Constraint = (Word8, Word8, Word8, Word8)
{- blocks:
    1 2 3
    4 5 6
    7 8 9
 -}

data T = T !Word8 !Word8
 deriving (Eq,Ord,Ix,Show)

moveConstraint (r,c,d) = (T r c, T r d, T c d, T (block9 r c) d)

block9 r c = 1+3*((c-1) `div` 3) + (r-1) `div` 3

type BoolSet = Array Word8 Bool

makeMove (r,c,d) (rcs,rds,cds,bds) | not (rcs!rc) && not (rds!rd) && not (cds!cd) && not (bds!bd) = Just (ins rcs rc, ins rds rd, ins cds cd, ins bds bd)
 where (rc,rd,cd,bd) = moveConstraint (r,c,d)
makeMove _ _ = Nothing

makeMove' (r,c,d) (rcs,rds,cds,bds) | not (rcs!rc) && not (rds!rd) && not (cds!cd) && not (bds!bd) = (ins rcs rc, ins rds rd, ins cds cd, ins bds bd)
 where (rc,rd,cd,bd) = moveConstraint (r,c,d)

ins arr e = arr // [(e,True)]

emptyArray = listArray (T 1 1, T 9 9) [False,False ..]
emptyState = (emptyArray, emptyArray, emptyArray, emptyArray)

makeMoves [] st = st
makeMoves (((r,c),d):ps) st =
  case makeMove (r,c,d) st of
   Nothing -> st
   Just st' -> makeMoves ps st'

validMoves (rca,rda,cda,bda) = [ (r,c,d) | (r,c,d) <- range ((1,1,1),(9,9,9)), not (rca!(T r c)), not (rda!(T r d)), not (cda!(T c d)), not (bda!(T (block9 r c) d)) ]


solve ms = map fst $ fst $ gen_boards' (81-length ms) ms

test_solve = do
  as <- getArgs
  g <- case as of
        (a:_) -> return (mkStdGen (read a))
        _ -> do g' <- newStdGen
                let (i,_) = randomR (0,10000) g'
                printf "Random seed: %d\n" i
                return (mkStdGen i)
  let (_,(ps:_)) = prefill_ (mkStdGen 2) 20
  let ps' = map fst ps
  prettyBoardIO (empty_board // ps')
  let st = makeMoves ps' emptyState

  let (_,(ms:_)) = gen_boards''' (repeat id) 0 1000 (length ps') (read (head as)) st []
  foreachM (tail $ inits ms) $ \ms -> do
   let (m,vs) = last ms
   printf "Move %d: %s\n" (length ms) (show m)
   printf "valid moves: %d %s\n" (length vs) (show vs)
   prettyBoardIO $ empty_board // (ps' ++ map fst ms)

--  prettyBoardIO $ empty_board // (head $ gen_boards''' 0 66 emptyState [])
{-
  mapM (prettyBoardIO . (empty_board//)) (take 5 (gen_boards'' 10 emptyState []))
  ms <- generate_board g 2
  let b = empty_board // ms
  prettyBoardIO b
  print "solving..."
  let solution_mods = solve ms
  let solutions = map (b//) solution_mods
  let n = length solution_mods
  print (head solution_mods)
  putStrLn $ prettyBoard (head solutions)
  print n
  return b
-}

combine b1 b2 = b1 // (filter ((/=0) . snd) (assocs b2))

fmt_time :: Int -> String
fmt_time tm = printf "%02d:%02d:%02d"
                          (tm `div` 3600)
                          ((tm `div` 60) `mod` 60)
                          (tm `mod` 60)

 
s_main = do
  test_solve


