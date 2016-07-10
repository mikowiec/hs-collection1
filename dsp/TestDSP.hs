
module Main where


import WAV

import System

import Data.Array.Unboxed
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.IO
import Data.Word

import Numeric.Transform.Fourier.SlidingFFT
import Numeric.Transform.Fourier.DFT
import DSP.Source.Oscillator
import DSP.Estimation.Frequency.FCI
import DSP.Filter.Analog.Prototype
import DSP.Filter.IIR.IIR

import Complex


notch :: Floating a => a -> a -> [a] -> [a]
notch bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
    where b0 =   1
          b1 =  -2 * cos w
          b2 =   1
          a0 =   1 + alpha
          a1 =  -2 * cos w
          a2 =   1 - alpha
	  alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)


notchFilter wav@(Wav fmt (DataChunk bytes0)) fq = do
    bytes <- convArray8to16 bytes0
    let ys = map fromIntegral (elems bytes) :: [Int]
    let xs = map fromIntegral (ys) :: [Float]
    let w = 2*pi*fq/(fromIntegral (sample_rate fmt))
    let bw = 2
    let n = snd (bounds bytes) + 1
    let xs' = map ((/(2^15))) xs
    let xs'' = notch bw w xs' :: [Float]
    let xs''' = (map (round . (*2^15)) xs'') :: [Int]
    bytes''' <- convArray16to8 (listArray (0,n-1) (map (fromIntegral) xs'''))
    return $ Wav fmt (DataChunk bytes''')

genWavHz n hz = do
--    let xs = take n $ nco (2*pi*hz/44100) 1.0 :: [Double]
    let xs = map (\i -> sin (2*pi*hz*((fromIntegral i)/44100))) [0..n-1]
    let xs' = (map (round . (*(2^15))) xs)
    arr <- array16OfList n xs'
    arr' <- convArray8to16 arr
    let ys = elems arr'
    let fmt = FmtChunk 1 1 44100 88200 2 16
    return $ Wav fmt (DataChunk arr)

addWav w1@(Wav fmt (DataChunk bytes1)) w2@(Wav _ (DataChunk bytes2)) = do
    let n1 = snd (bounds bytes1) + 1
        n2 = snd (bounds bytes2) + 1
        n = min n1 n2
    let arr = listArray (0,n-1) (zipWith (+) (elems bytes1) (elems bytes2))
    return $ Wav fmt (DataChunk arr)

main = do
    pn <- getProgName
    as <- getArgs
    case as of
     
     (["-i",input]) -> do
        wav@(Wav fmt dt) <- readWav input
        print fmt
     
     (["-s",input,outputbase]) -> do
        wav <- readWav input
        (left,right) <- splitWav wav
        writeWav (outputbase++"-left.wav") left
        writeWav (outputbase++"-right.wav") right
     
     (["-m",left,right,output]) -> do
        lw <- readWav left
        rw <- readWav right
        wav <- mergeWav lw rw
        writeWav output wav
     
     (["-f",input,output,hz]) -> do
        wav <- readWav input
        wav' <- notchFilter wav (read hz)
        writeWav output wav'

     (["-o",output,n,hz]) -> do
        wav <- genWavHz (read n) (read hz)
        writeWav output wav
        
     (["-a",input1,input2,output]) -> do
        w1 <- readWav input1
        w2 <- readWav input2
        ow <- addWav w1 w2
        writeWav output ow
     
     (["-c",input,output,start,n]) -> do
        wav <- readWav input
        wav' <- cropWav wav (read start) (read n)
        writeWav output wav'
     _ -> do
      putStr ("Usage: "++pn++"\n" ++
              "    -i input                       Print wav-info\n" ++
              "    -s input outputbase            Split into left&right channels\n"++
              "    -m left right output           Merge channels\n"++
              "    -c input output start samples  Crop\n"++
              "    -h help\n"++
              "\n")
    
