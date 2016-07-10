
module DancingLinks where

import Data.IORef
import Data.Array
import Control.Monad.Fix

import Data.Array.MArray
import Data.Array.IO

import Maybe
import Monad
import Text.Printf

import Misc

type Ref a = IORef a

newRef v = newIORef v
writeRef r v = writeIORef r v
readRef r = readIORef r

data Payload
 = MatrixCell {
    cell_id :: (Int,Int),
    cell_hdr_ref :: Ref Cell
 }
 | HdrCell {
    hdr_id :: Int,
    hdr_size :: Ref Int
 }
 | HdrHeadCell

data Cell = Cell {
  up :: Ref Cell,
  down :: Ref Cell,
  left :: Ref Cell,
  right :: Ref Cell,
  payload :: Payload
 }

instance Show Cell where
 show (Cell u d l r (MatrixCell (row,col) _)) = printf "MatrixCell (%d,%d)" row col
 show (Cell u d l r (HdrCell id _)) = printf "HdrCell %d" id
 show (Cell u d l r (HdrHeadCell)) = "HdrHeadCell"

unlink (Cell u d l r _) = do
  lc <- readRef l
  rc <- readRef r
  uc <- readRef u
  dc <- readRef d
  writeRef (right lc) rc
  writeRef (left rc) lc
  writeRef (down uc) dc
  writeRef (up dc) uc

relink c@(Cell u d l r _) = do
  lc <- readRef l
  rc <- readRef r
  uc <- readRef u
  dc <- readRef d
  writeRef (right lc) c
  writeRef (left rc) c
  writeRef (down uc) c
  writeRef (up dc) c

type RowIx = (Int,Int,Int)
type ColIx = Int
type MtxIx = (RowIx,ColIx)
type DLArray = Array MtxIx (Maybe Cell)

data DLMatrix = DLMatrix {
  header :: Cell,
  all_headers :: [Cell],
  dl_array :: DLArray 
 }
 

block9 r c = 3*(c `div` 3) + r `div` 3


hdr_ix_of_move r c d = 
  let b = block9 r c
      h1 = 9*r+c
      h2 = 9*r+d
      h3 = 9*c+d
      h4 = 9*b+d
  in (h1,81+h2,2*81+h3,3*81+h4)


makeMove hs r c d = do
  
  return () -- return undo

validMoves hs = do
  return [] -- [(r,c,d)]

moveIsPossible hs r c d = do
  return False

validNums hs r c = do
  return []

select_col h@(Cell _ _ l r (HdrCell id _)) = select' h h
 where select' selected h = do
        h' <- readRef (right h)
        if isHeaderCell h'
         then best_of selected h' (\h -> select' h h')
         else return selected
       best_of h1 h2 k = do
        n1 <- readRef (hdr_size (payload h1))
        n2 <- readRef (hdr_size (payload h2))
        k (if n1 <= n2 then h1 else h2)
        

solve_step (DLMatrix hdr hs array) = do
  print hdr
  h <- readRef (right hdr)
  print h
  hdr' <- select_col h
  printf "selected %s\n" (show hdr')
  return hdr'

with_move (DLMatrix hdr hs array) hdr' op = do
  -- unlink col
  -- unlink row
  -- op
  -- relink row
  -- relink col

initState = do
  cs <- foreachM [0..728] $ \r -> mfix $ \(~(c1,c2,c3,c4)) -> do
    u1 <- newRef c1
    u2 <- newRef c2
    u3 <- newRef c3
    u4 <- newRef c4
    d1 <- newRef c1
    d2 <- newRef c2
    d3 <- newRef c3
    d4 <- newRef c4
    l1 <- newRef c4
    l2 <- newRef c1
    l3 <- newRef c2
    l4 <- newRef c3
    r1 <- newRef c2
    r2 <- newRef c3
    r3 <- newRef c4
    r4 <- newRef c1
    ref <- newRef undefined
    return (Cell u1 d1 l1 r1 (MatrixCell (r,0) ref),
            Cell u2 d2 l2 r2 (MatrixCell (r,1) ref),
            Cell u3 d3 l3 r3 (MatrixCell (r,2) ref),
            Cell u4 d4 l4 r4 (MatrixCell (r,3) ref))
  array <- newArray (((0,0,0),0), ((8,8,8),323)) Nothing :: IO (IOArray ((Int,Int,Int),Int) (Maybe Cell))
  foldM (\array ((r,c,d),cells) -> do
        let (c1,c2,c3,c4) = cells
            row = (r,c,d)
            b = block9 r c
            (h1,h2,h3,h4) = hdr_ix_of_move r c d
        mapM (\(ix,e) -> writeArray array ix e)
              [((row,h1), Just c1),
               ((row,h2), Just c2),
               ((row,h3), Just c3),
               ((row,h4), Just c4)]
        return array) array (zip row_indices cs)
  array' <- freeze array

  hs <- mfix $ \hs -> do
   foreachM [0..324] $ \c -> do
    -- fetch cells, unless "header head"
    hl <- newRef (hs!!((c-1)`mod`325))
    hr <- newRef (hs!!((c+1)`mod`325))
    n <- newRef 9
    if c < 324
     then do
      hdr <- mfix (\cell -> newRef cell >>= \r -> return (Cell r r hl hr (HdrCell c n)))
      let cells = [ cell | Just cell <- map (\ix -> array'!(ix,c)) row_indices ]
      let (c1:c2:cs) = cells
      foreachM_ (zip3 (hdr:cells) (cells ++ [hdr]) (tail cells ++ [hdr,c1])) $
       \(cu,Cell u d _ _ payload,cd) -> do
        writeRef u cu
        writeRef d cd
        case payload of
         MatrixCell _ ref -> writeRef ref hdr
         _ -> return ()
      return hdr
     else do
      hdr <- mfix (\cell -> newRef cell >>= \r -> return (Cell r r hl hr (HdrHeadCell)))
      return hdr

  return (DLMatrix (last hs) (init hs) array')
  
row_indices = [(r,c,d) | r <- [0..8], c <- [0..8], d <- [0..8] ]

isHeaderCell (Cell _ _ _ _ (HdrCell _ _)) = True
isHeaderCell _ = False

check = do
  st@(DLMatrix hdr_cell hdr array) <- initState
  foreachM row_indices $ \row_ix -> do
    printf "%s" (show row_ix)
    foreachM [0..323] $ \col -> do
      when (isJust (array!(row_ix,col))) $ do
        printf " %3d" col
    putStrLn ""
  print 20

  foreachM hdr $ \(Cell _ d l r (HdrCell _ _)) -> do
   first_cell <- readRef d
   let loop :: IORef Cell -> Int -> IO Int
       loop cr n = do
        cell <- readRef cr
        if isHeaderCell cell
          then return n
          else loop (down cell) (n+1)
   n <- loop d 0
   when (n/=9) $ print n
   return ()
  return st

dl_main = do
  st <- check
  solve_step st
  solve_step st
  solve_step st
  solve_step st
  solve_step st
  solve_step st


