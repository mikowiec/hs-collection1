
module WAV where

import Data.Bits
import Data.Word
import Data.Int
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.IO
import Data.Array.ST
import System.IO
import Monad
import Char

type Bytes = UArray Int Word8
type WavData = UArray Int Word8

type Byte4x8 = [Word8]

data Format = WAVE

data Chunk = Chunk {
    chunk_id :: Byte4x8,
    chunk_size :: Word32,
    chunk_data :: Bytes
  }
 deriving Show

data Wav = Wav FmtChunk DataChunk
 deriving Show

data FmtChunk = FmtChunk {
    audio_format :: Int,
    num_channels :: Int,
    sample_rate :: Int,
    byte_rate :: Int,
    block_align :: Int,
    bits_per_sample :: Int
  }
 deriving Show

data DataChunk = DataChunk {
    bytes :: Bytes
  }
 deriving Show
    

leWord arr offs bytes =
   fromIntegral $ foldr f 0 [0..bytes-1]
 where f k n = n + 2^(k*8) * (fromIntegral (arr!(offs+k))::Word32)

parseFmt :: Chunk -> FmtChunk
parseFmt (Chunk id sz dt) = 
  FmtChunk
    (leWord dt 0 2)
    (leWord dt 2 2)
    (leWord dt 4 4)
    (leWord dt 8 4)
    (leWord dt 12 2)
    (leWord dt 14 2)

parseData :: Chunk -> DataChunk
parseData (Chunk id sz dt) = 
  DataChunk dt
  
err m = hPutStrLn stderr $ show m

read4 h = do
    arr <- newArray_ (0,3)
    4 <- hGetArray h arr 4
    unsafeFreeze arr

readHeader h = do
    hdr <- newArray_ (0,7) :: IO (IOUArray Int Word8)
    8 <- hGetArray h hdr 8
    arr <- unsafeFreeze hdr :: IO (UArray Int Word8)
    let id = map (arr!) [0..3]
    let id' = map (chr.fromIntegral) id
    let sz = fromIntegral $ leWord arr 4 4 :: Word32
    return (id,fromIntegral sz)

readChunk h = do
    (id,sz) <- readHeader h
    dt <- newArray_ (0,sz-1) :: IO (IOUArray Int Word8)
    hGetArray h dt (fromIntegral sz)
    dt' <- freeze dt :: IO (UArray Int Word8)
    return (Chunk id (fromIntegral sz) dt')


readWav :: FilePath -> IO Wav
readWav fp = do
    h <- openBinaryFile fp ReadMode
    main <- readHeader h
    format <- read4 h :: IO Bytes
    fmt <- readChunk h
    dt <- readChunk h
    hClose h
    return (Wav (parseFmt fmt) (parseData dt))

c2b c = fromIntegral (ord c)
i2le32 i = [ (i `shiftR` s) .&. 0xff | s <- [0,8,16,24] ]
i2le16 i = [ (i `shiftR` s) .&. 0xff | s <- [0,8] ]

writeWav :: FilePath -> Wav -> IO ()
writeWav fp (Wav fmt dt) = do
    h <- openBinaryFile fp WriteMode
    let hdr_size = 4 + (8 + 16) + 8
        dt_size = snd (bounds (bytes dt)) + 1
        total_size = hdr_size + dt_size
    let header = map c2b "RIFF" ++
                 i2le32 total_size ++
                 map c2b "WAVE" ++
                 map c2b "fmt " ++
                 i2le32 16 ++
                 i2le16 (audio_format fmt) ++
                 i2le16 (num_channels fmt) ++
                 i2le32 (sample_rate fmt) ++
                 i2le32 (byte_rate fmt) ++
                 i2le16 (block_align fmt) ++
                 i2le16 (bits_per_sample fmt) ++
                 map c2b "data" ++
                 i2le32 dt_size
    hdr <- newListArray (0,length header - 1) (map fromIntegral header)
    hPutArray h hdr (length header)
    dt' <- unsafeThaw (bytes dt)
    hPutArray h dt' (dt_size)
    hClose h
    return ()


half_ch fmt = fmt { num_channels = 1, byte_rate = byte_rate fmt `div` 2, block_align = block_align fmt `div` 2 }


idx_gen :: (Int->Bool) -> Int -> [Int]
idx_gen f d = 
  filter (\c -> f (c `div` d)) [0..]

mkHalfArray arr ix_map = do
    let new_len = (snd (bounds arr) + 1) `div` 2
    arr' <- newArray (0,new_len-1) 0 :: IO (IOUArray Int Word8)
    let f i = writeArray arr' i (arr!(ix_map i))
    mapM_ f [0..new_len-1]
    freeze arr' :: IO (UArray Int Word8)

splitWav :: Wav -> IO (Wav,Wav)
splitWav (Wav fmt dt) = do
    let org_arr = bytes dt
    let n = snd (bounds org_arr) + 1
    let bps = (bits_per_sample fmt `div` 8) :: Int
    let new_len = n `div` 2
    let left_map i = i `mod` 2 + (i `div` 2) * 4
    l_arr <- mkHalfArray org_arr left_map
    r_arr <- mkHalfArray org_arr (\i -> 2 + left_map i)
    let left = Wav (half_ch fmt) (DataChunk l_arr)
        right = Wav (half_ch fmt) (DataChunk r_arr)
    return (left,right)


cropWav :: Wav -> Int -> Int -> IO Wav
cropWav wav@(Wav fmt dt) start len = do
    let samples = block_align fmt * len
    let start_sample = block_align fmt * start
    arr <- newListArray (0,samples-1) (drop start_sample $ take samples $ elems (bytes dt))
    arr' <- unsafeFreeze (arr :: IOUArray Int Word8)
    return $ Wav fmt (DataChunk arr')

mergeWav left@(Wav fmt dt) right@(Wav fmt' dt') = do
    let new_fmt = 
         fmt { num_channels = 2,
               byte_rate = byte_rate fmt * 2,
               block_align = block_align fmt * 2 }
    let orig_len = (snd (bounds (bytes dt))) + 1
    let new_range = (0, (2*orig_len) - 1)
    let left_bytes = bytes dt; right_bytes = bytes dt'
    let get_byte i =
         (if even (i`div`2) then (\x->(left_bytes!x)) else (\x->(right_bytes!(x-1))))
            (i `div` 2 + i `mod` 2)
    arr <- newListArray new_range [get_byte i | i <- [0..] ]  :: IO (IOUArray Int Word8)
    arr' <- unsafeFreeze arr :: IO (UArray Int Word8)
    return (Wav new_fmt (DataChunk arr'))


arrayOfList n xs = do
    arr <- newListArray (0,n-1) xs :: IO (IOUArray Int Word8)
    arr' <- unsafeFreeze arr :: IO (UArray Int Word8)
    return arr'

convArray8to16 :: UArray Int Word8 -> IO (UArray Int Int16)
convArray8to16 arr = do
    let n = snd (bounds arr) + 1
        n_2 = n `div` 2
    let acc :: [Word8] -> [Int16]
        acc [] = []
        acc (x:y:xs) = (fromIntegral x + (fromIntegral y `shiftL` 8)) : acc xs
    arr' <- newListArray (0,n_2-1) (acc (elems arr)) :: IO (IOUArray Int Int16)
    arr'' <- unsafeFreeze arr'
    return arr''

convArray16to8 :: UArray Int Int16 -> IO (UArray Int Word8)
convArray16to8 arr = do
    let n = snd (bounds arr) + 1
        n_2 = n * 2
    let acc :: [Int16] -> [Word8]
        acc [] = []
        acc (x:xs) = 
                fromIntegral (x .&. 0xff) :
                fromIntegral ((x .&. 0xff00) `shiftR` 8) :
                acc xs
    arr' <- newListArray (0,n_2-1) (acc (elems arr)) :: IO (IOUArray Int Word8)
    arr'' <- unsafeFreeze arr'
    return arr''

array16OfList :: Int -> [Int16] -> IO (UArray Int Word8)
array16OfList n xs = do
    arr' <- convArray16to8 (listArray (0,n-1) xs) :: IO (UArray Int Word8)
    return arr'
    

