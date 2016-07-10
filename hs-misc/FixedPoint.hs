
module FixedPoint where

import Data.Bits
import Data.Int
import Data.Word


data Int16x16 = Int16x16 Int16 Word16
 deriving (Eq,Ord,Show)


instance Num Int16x16 where
 a + b = i32toX $ x2i32 a + x2i32 b
 a * b = i32toX $ fromIntegral $ (x2i64 a * x2i64 b) `shiftR` 16
 abs (Int16x16 a b) = Int16x16 (abs a) b
 fromInteger i = Int16x16 (fromInteger i) 0
 signum (Int16x16 a _) | a > 0 = fromInteger 1
 signum (Int16x16 0 _) = fromInteger 0
 signum (Int16x16 a _) | a < 0 = fromInteger (-1)


x2i32 :: Int16x16 -> Int32
x2i32 (Int16x16 a b) = fromIntegral a `shiftL` 16 + fromIntegral b

x2w32 :: Int16x16 -> Word32
x2w32 (Int16x16 a b) = fromIntegral a `shiftL` 16 + fromIntegral b

x2i64 :: Int16x16 -> Int64
x2i64 (Int16x16 a b) = fromIntegral a `shiftL` 16 + fromIntegral b

x2w64 :: Int16x16 -> Word64
x2w64 (Int16x16 a b) = fromIntegral a `shiftL` 16 + fromIntegral b

i32toX :: Int32 -> Int16x16
i32toX i = Int16x16 (fromIntegral (i `shiftR` 16)) (fromIntegral i .&. 0xffff)

w32toX :: Word32 -> Int16x16
w32toX i = Int16x16 (fromIntegral (i `shiftR` 16)) (fromIntegral i .&. 0xffff)

