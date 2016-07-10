
module Property where

import Types

data PropType
    = PropInt Int
    | PropFloat Float
    | PropVector Vector
    | PropString String
 deriving (Read, Show)

class PropRep s where
    to_prop :: s -> [(String,PropType)]
    merge_prop :: [(String,PropType)] -> s -> s

std_to_prop s = [("<all>", PropString (show s))]
std_merge_prop [(_,PropString s)] s' =
    case reads s of
     [(a,[])] -> a
     _ -> s'
std_merge_prop _ s' = s'


type Properties = [(String, PropType)]

