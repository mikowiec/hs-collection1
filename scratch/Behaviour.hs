
module Behaviour where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans
import Action

type BehaviourM a = StateT Behaviour Identity a

list_behaviour :: Event -> BehaviourM ()
list_behaviour "init" = initial_state
list_behaviour "down" = do
  b <- get
  if list_focus b == list_size b then do_focus_first else do_focus_next

initial_state :: BehaviourM ()
initial_state = put (Behaviour 0 0)
do_focus_first = modify_focus (const 0)
do_focus_next = modify_focus succ

data Behaviour = Behaviour {
    list_focus :: Int,
    list_size :: Int
  }


modify_focus :: (Int -> Int) -> BehaviourM ()
modify_focus f = modify (\b -> b  { list_focus = f (list_focus b) } )

