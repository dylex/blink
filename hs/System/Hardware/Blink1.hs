module System.Hardware.Blink1
  ( RGB(..)
  , Delay(..)
  , PatternStep

  , getVersion
  , setColor
  , fadeToColor
  , setServerDown
  , playPattern
  , setPattern
  , getPattern
  , getSerialNum
  , setSerialNum
  , testBlink1
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import Data.Bits (shiftR, shiftL, (.|.))
import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Word (Word16, Word32)
import System.Hardware.Blink1.Class
import System.Hardware.Blink1.Types

reportId :: Word8
reportId = 1

msgLen :: Int
msgLen = 8

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fill :: Int -> a -> [a] -> [a]
fill 0 _ [] = []
fill 0 _ _ = error "fill: list too long"
fill n x [] = replicate n x
fill n x (a:l) = a : fill (pred n) x l

command :: Blink1 b => b -> Char -> [Word8] -> IO ()
command b c d = writeBlink1 b (reportId : fi (ord c) : fill (pred msgLen) 0 d)

request :: Blink1 b => b -> Char -> [Word8] -> IO [Word8]
request b c d = do
  command b c d
  threadDelay 50000 -- FIXME says the original
  tail `liftM` readBlink1 b (succ msgLen)

getVersion :: Blink1 b => b -> IO (Char,Char)
getVersion b = do
  _:_:maj:min:_ <- request b 'v' []
  return (chr (fi maj), chr (fi min))

rgb :: RGB -> [Word8]
rgb (RGB r g b) = [r,g,b]

delay :: Delay -> [Word8]
delay d = [i $ t `shiftR` 8, i t] where 
  t = truncate (100 * d) :: Word16
  i = fi :: Word16 -> Word8

pos :: PatternStep -> [Word8]
pos p = [fi (fromEnum p)]

-- | set the given color now
setColor :: Blink1 b => b -> RGB -> IO ()
setColor b c = command b 'n' $ rgb c

fadeToColor :: Blink1 b => b -> Delay -> RGB -> IO ()
fadeToColor b d c = command b 'c' $ rgb c ++ delay d

-- | enable/disable serverdown mode
setServerDown :: Blink1 b => b -> Bool -> Delay -> IO ()
setServerDown b o d = command b 'D' $ fi (fromEnum o) : delay d

-- | stop or start playing the sequence at the given position
playPattern :: Blink1 b => b -> Maybe PatternStep -> IO ()
playPattern b Nothing = command b 'p' [0]
playPattern b (Just p) = command b 'p' $ 1 : pos p

-- | set the sequence pattern for the given position
setPattern :: Blink1 b => b -> PatternStep -> Delay -> RGB -> IO ()
setPattern b p d c = command b 'P' $ rgb c ++ delay d ++ pos p

getPattern :: Blink1 b => b -> PatternStep -> IO (Delay, RGB)
getPattern b p = do
  _:r:g:b:d1:d2:_ <- request b 'R' $ rgb black ++ delay 0 ++ pos p
  return (fi (i d1 `shiftL` 8 .|. i d2) / 100, RGB r g b)
  where i = fi :: Word8 -> Word16

eeaddr :: EEPROMAddr -> Word8
eeaddr = fi . fromEnum

readEEPROM :: Blink1 b => b -> EEPROMAddr -> IO Word8
readEEPROM b a = do
  _:_:v:_ <- request b 'e' [eeaddr a]
  return v

writeEEPROM :: Blink1 b => b -> EEPROMAddr -> Word8 -> IO ()
writeEEPROM b a v = command b 'E' [eeaddr a, v]

getSerialNum :: Blink1 b => b -> IO Word32
getSerialNum b =
  foldl' (\l -> (l `shiftL` 8 .|.) . fi) 0 `liftM` 
    mapM (readEEPROM b . EESerialNum) [0..pred serialNumLen]

setSerialNum :: Blink1 b => b -> Word32 -> IO ()
setSerialNum b s = mapM_ w [0..pred serialNumLen] where
  w i = writeEEPROM b (EESerialNum i) $ fi $ s `shiftR` (8*(3-fi i))

testBlink1 :: Blink1 b => b -> IO (Either [Word8] Bool)
testBlink1 b = do
  r <- request b '!' []
  return $ case r of
    0x55:0xAA:u:_ -> Right (u /= 0)
    _ -> Left r
