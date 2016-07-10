
module SudokuUI where


import Graphics.UI.WX as WX
import Graphics.UI.WXCore as WXC

import Control.Concurrent

import Data.IORef
import Data.Array
import Text.Printf

import Char
import IO
import Random
import List

import Misc

import Sudoku


data Game = Game {
  level :: Int,  -- number of prefilled items
  game_pre   :: BoardMoves,
  game_usr   :: BoardMoves,
  game_slv   :: BoardMoves,
  game_sel   :: Maybe BoardPos
 }

seed_range = (0,10000)

item_size :: Int
item_size = 50

thin_border_width :: Int
thin_border_width = 2

thick_border_width :: Int
thick_border_width = 4

edge_border_width :: Int
edge_border_width = 5

item_offset_x = item_size `div` 3 + 3
item_offset_y = item_size `div` 3 - 3

decode_pos :: Int -> Int -> Maybe BoardPos
decode_pos x y = do
  let toPos i | i >= 0 && i <= 8 = Just (i+1)
      toPos _ = Nothing
  i <- toPos (x `div` item_size) 
  j <- toPos (y `div` item_size)
  return (fromIntegral i, fromIntegral j)


line_w i j  | i `elem` [1,9] && j `elem` [1,9]     = edge_border_width
line_w i j  | (i) `mod` 3 == 0 && (j) `mod` 3 == 0 = thick_border_width
line_w i j  | otherwise = thin_border_width


paint_board game dc viewArea = do
  mapM_ (\i -> ln (i,0) (i,9)) [ i | i <- [0..9] ]
  mapM_ (\j -> ln (0,j) (9,j)) [ j | j <- [0..9] ]
  Game _ pre usr slv sel  <- readIORef game
  let number ii jj clr n = let i = fromIntegral ii; j = fromIntegral jj in do
       let (x,y) = (item_offset_x + (i-1)*item_size, item_offset_y + (j-1)*item_size)
       drawText dc (show n) (pt x y) [fontFace := "Courier New", fontSize := 24, textColor := clr]
  withM sel $ \(s_i,s_j) -> do
    drawRect dc (Rect ((fromIntegral s_i-1)*item_size + 4) ((fromIntegral s_j-1)*item_size + 4) (item_size-6) (item_size-6))
                [ penColor := grey ]
  let (us_ok,us_err) = split_usr [] [] (empty_board // pre) (reverse usr)
  mapM_ (\((i,j),n) -> number i j black n) pre
  mapM_ (\((i,j),n) -> number i j blue n) us_ok
  mapM_ (\((i,j),n) -> number i j red n) us_err
  mapM_ (\((i,j),n) -> number i j green n) slv

 where 
  split_usr os es _ [] = (os,es)
  split_usr os es b (m:ms) | mod_is_ok b m = split_usr (m:os) es (b//[m]) ms
  split_usr os es b (m:ms) | otherwise     = split_usr os (m:es) (b//[m]) ms
  ln (i,j) (i',j') = do
   let [x,y,x',y'] = map (*item_size) [i,j,i',j']
   line dc (pt x y) (pt x' y') [penKind := PenSolid, penWidth := line_w  i j ] 

sui_main = start $ do
  rnd <- newStdGen
  rnd_ref <- newIORef rnd
  let mkGen = do
        g <- readIORef rnd_ref
        let (g1,g2) = split g
        writeIORef rnd_ref g2
        return g1
  game_ref <- newIORef (Game 2 [] [] [] Nothing)
  time_ref <- newIORef 0
  selected_ref <- newIORef (Nothing :: Maybe BoardPos)

  f <- frame [ text := "Sudoku!" ]
  brd <- window f [ size := sz (9*item_size + 2) (9*item_size + 2),
                    bgcolor := white,
                    on paint := paint_board game_ref ]

  new_game_btn <- button f [ text := "New Game" ]
  undo_btn <- button f [ text := "Undo" ]
  solve_btn <- button f [ enabled := False ]

  select_btn <- button f [ text := "Select Game" ]

  level_select <- choice f [ items := ["1","2","3","4","5"] ]
  sel <- level $^ readIORef game_ref
  set level_select [ selection := sel-1 ]

  set level_select [ on select :=
    do s <- get level_select selection 
       modifyIORef game_ref (\g -> g { level = s+1 }) ]

  time_txt <- staticText f [  ]
  time_timer <- timer time_txt [ enabled := False, interval := 1000 ]
  set time_timer [ on command := do modifyIORef time_ref succ
                                    tm <- readIORef time_ref
                                    set time_txt [ text := fmt_time tm ] ]

  game_txt <- staticText f []

  set brd [ on click := \(Point x y) -> do 
    case decode_pos x y of
     Just (i,j) -> do modifyIORef game_ref (\g -> g {
                        game_sel = maybe (Just (i,j)) (\_->Nothing) (game_sel g) } )
                      repaint brd
     _ -> writeIORef selected_ref Nothing
   ]

  set brd [ on (charKey ' ') := do
    modifyIORef game_ref $ \g ->
      g { game_usr = maybe (game_usr g)
                           (\sel -> filter ((/=sel) . fst) (game_usr g))
                           (game_sel g),
          game_sel = Nothing } ]

  let make_move n = do
      game <- readIORef game_ref
      withM (game_sel game) $ \sel -> do
        writeIORef game_ref (game { game_usr = (sel,n) : (filter ((/=sel).fst) (game_usr game)), game_sel = Nothing } ) 
        repaint brd

  foreachM ['1'..'9'] $ \c ->
    set brd [ on (charKey c) := make_move (fromIntegral (ord c - ord '0')) ]

  let start_new_game seed = do
       game <- readIORef game_ref 
       set new_game_btn [ enabled := False ]
       set game_txt [ text := (show seed) ]
       moves <- generate_board (mkStdGen seed) (level game)
       modifyIORef game_ref (\g -> g { game_pre = moves, game_usr = [], game_slv = [], game_sel = Nothing })
       hide_sol
       set new_game_btn  [ enabled := True ]
       set solve_btn     [ enabled := True ]
       writeIORef time_ref 0
       set time_timer [ enabled := True ]
       repaint brd

      new_game = do
       seed <- randomRIO seed_range
       start_new_game seed


      select_game = do
       d <- dialog f []
       ok <- button d [ text := "Start" ]
       seed <- randomRIO seed_range 
       seed_entry <- textEntry d [ text := show seed ]
       set d [ layout := margin 10 $ column 5 [ row 5 [ label "Seed:", widget seed_entry ], alignRight (widget ok) ] ]
       seed <- showModal d (\stop -> set ok [ on command := get seed_entry text >>= stop . Just ] )
       withM seed $ \s -> case reads s of
                           [(n,[])] -> start_new_game n
                           _ -> return ()
       return ()

      solve_game = do
       Game _ pre usr _ _ <- readIORef game_ref
       set time_timer [ enabled := False ]
       let board_usr = (empty_board // pre // usr)
       let ms = solve (pre++usr)
       case (is_valid board_usr,ms) of
        (False,_) -> do print "invalid board"
        (True,m:_) -> do
                    modifyIORef game_ref (\g -> g { game_slv = m })
                    set solve_btn [ text := "Hide solution", on command := hide_sol ]
                    repaint brd
        _ -> return ()
      hide_sol = do
       modifyIORef game_ref (\g -> g { game_slv = [] })
       set solve_btn [ text := "Find solution", on command := solve_game ]
       repaint brd

  hide_sol

  
  let undo_move = do
       modifyIORef game_ref (\g -> g { game_usr = safeTail (game_usr g) }) 
       repaint brd

  set undo_btn      [ on command := undo_move ]
  set new_game_btn  [ on command := new_game ]
  set select_btn    [ on command := select_game ]
  set solve_btn     [ on command := solve_game ]
  set f [ layout := (margin 10 (column 5
    [row 5 [widget new_game_btn, 
            widget select_btn,
            hspace 10,
            widget undo_btn,
            hspace 10, 
            alignBottom (label "Level:"), widget (level_select), glue],
     margin 15 (widget brd),
     row 5 [label "Time:", fill (widget time_txt), 
            label "Game:", fill (widget game_txt), glue,
            fill (widget solve_btn)]])) ]


  
