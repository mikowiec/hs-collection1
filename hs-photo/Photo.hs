
module Photo where

import Graphics.Rendering.OpenGL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Control.Exception
import Control.Concurrent
import Char
import Monad
import System.Process
import System.IO
import System.Exit
import Data.Word
import Foreign.Ptr
import Text.Printf

import System.Posix.Signals

thumbnail raw = ("exiftool", ["-b","-ThumbNailImage", raw])
preview raw = ("exiftool", ["-b","-PreviewImage", raw])
djpeg = ("djpeg", ["-pnm","-nosmooth"])
tmpDir = "/tmp"

loadPreview raw = do
  pm <- newMVar Nothing
  jm <- newMVar Nothing

  (c,bs,err) <- uncurry run (preview raw) Nothing
  when (c /= ExitSuccess) $ BS.hPut stderr err

  (c,bs,err) <- uncurry run djpeg (Just bs)
  when (c /= ExitSuccess) $ BS.hPut stderr err

  return bs

parsePPM :: BS.ByteString -> Maybe (Int,Int,BS.ByteString)
parsePPM bs = do
  let (magic,bs') = BS.splitAt 2 bs
  if magic == magic_P6
   then
    let ([_,width,_,height,_,maxValue,_],bs'') =
          parseBS [] [w8IsSpace,w8IsDigit,w8IsSpace,w8IsDigit,w8IsSpace,w8IsDigit,w8IsSpace] bs'
    in Just (bsReadInt width, bsReadInt height, bs'')
   else Nothing

parseBS rs [] bs = (reverse rs,bs)
parseBS rs (p:ps) bs = 
  let (r, bs') = BS.span p bs
  in parseBS (r:rs) ps bs'

magic_P6 = BS.pack (map (fromIntegral . ord) "P6") 

bsReadInt = read . map (chr . fromIntegral) . BS.unpack
w8IsDigit :: Word8 -> Bool
w8IsDigit = isDigit . chr . fromIntegral
w8IsSpace :: Word8 -> Bool
w8IsSpace = isSpace . chr . fromIntegral

texBindImage (width,height,bs) = do
  BSU.unsafeUseAsCString bs $ \ptr -> 
    texImage2D Nothing NoProxy 0 RGB8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 (PixelData RGB UnsignedByte ptr)

loadImage path = do
  return ()

run cmd args input = do
  mo <- newEmptyMVar
  me <- newEmptyMVar 
  (i,o,e,h) <- runInteractiveProcess cmd args Nothing Nothing
  forkIO (BS.hGetContents e >>= putMVar me >> hClose e)
  forkIO (BS.hGetContents o >>= putMVar mo >> hClose o)
  case input of
   Just bs -> forkIO (try (BS.hPut i bs) >> hClose i) >> return ()
   Nothing -> hClose i
  res <- waitForProcess h
  out <- takeMVar mo
  err <- takeMVar me
  return (res,out,err)




