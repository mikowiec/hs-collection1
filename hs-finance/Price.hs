
module Price where

import Data.Fixed
import Text.Printf

data Price = Price Int Int
 deriving (Eq,Ord)

instance Show Price where
 show (Price h l) = printf "%d.%02d" h l

priceToFloat (Price h l) = fromIntegral h + fromIntegral l / 100

instance HasResolution Price where
 resolution _ = 100

mkDec :: Int -> Int -> Fixed Price
mkDec v f = fromRational ((fromIntegral v * 100 + fromIntegral f) / 100)

