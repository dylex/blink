{-|

  To use any of these functions, you first must open a blink(1) device (providing the 'System.Hardware.Blink1.Class.Blink' interface).
  Currently "System.Hardware.Blink1.Linux" and "System.Hardware.Blink1.USB" are provided.

  The functions ending with 2 provide functionality available on the blink(1) mk2, so are only likely to work if 'getVersion' returns @('2',_)@

-}

module System.Hardware.Blink1
  ( RGB(..), RGB8
  , Delay(..)
  , PatternStep
  , LED

  , closeBlink1
  , getVersion
  , getColor2
  , setColor
  , setColor2
  , fadeToColor
  , fadeToColor2
  , setServerDown
  , setServerDown2
  , playPattern
  , playPattern2
  , getPlaying2
  , setPattern
  , getPattern
  , savePatterns2
  , getSerialNum
  , setSerialNum
  , testBlink1
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import Data.Bits (shiftR, shiftL, (.|.))
import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Word
import System.Hardware.Blink1.Class
import System.Hardware.Blink1.Types

reportId :: Word8
reportId = 1

msgLen :: Int
msgLen = 7

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
  _:_:mj:mn:_ <- request b 'v' []
  return (chr (fi mj), chr (fi mn))

rgb :: RGB8 -> [Word8]
rgb (RGB r g b) = [r,g,b]

delay :: Delay -> [Word8]
delay d = [i $ t `shiftR` 8, i t] where 
  t = truncate (100 * d) :: Word16
  i = fi :: Word16 -> Word8

bool :: Bool -> Word8
bool = fi . fromEnum

pos :: PatternStep -> Word8
pos = patternStep

led :: Maybe LED -> Word8
led = maybe 0 whichLED

-- | query the current color.
getColor2 :: Blink1 b => b -> LED -> IO RGB8
getColor2 dev n = do
  _:r:g:b:_ <- request dev 'r' [0,0,0,0,0,led (Just n)]
  return $ RGB r g b

-- | set the given color now
setColor :: Blink1 b => b -> RGB8 -> IO ()
setColor b = setColor2 b Nothing

-- | Although documented, this does not appear to work correctly.
setColor2 :: Blink1 b => b -> Maybe LED -> RGB8 -> IO ()
setColor2 b n c = command b 'n' $ rgb c ++ [0,0,led n]

fadeToColor :: Blink1 b => b -> Delay -> RGB8 -> IO ()
fadeToColor b = fadeToColor2 b Nothing

fadeToColor2 :: Blink1 b => b -> Maybe LED -> Delay -> RGB8 -> IO ()
fadeToColor2 b n d c = command b 'c' $ rgb c ++ delay d ++ [led n]

-- | enable/disable serverdown mode with the given timeout
setServerDown :: Blink1 b => b -> Bool -> Delay -> IO ()
setServerDown b o d = setServerDown2 b o d False (PatternStep 0, PatternStep 0)

-- | enable/disable serverdown mode with the given timeout, optionally staying on afterwards, over the given pattern range
setServerDown2 :: Blink1 b => b -> Bool -> Delay -> Bool -> (PatternStep, PatternStep) -> IO ()
setServerDown2 b o d s (sp,ep) = command b 'D' $ bool o : delay d ++ [bool s, pos sp, pos ep]

-- | stop or start playing the sequence at the given position
playPattern :: Blink1 b => b -> Maybe PatternStep -> IO ()
playPattern b Nothing = command b 'p' [0]
playPattern b (Just p) = command b 'p' [1, pos p]

-- | loop the sequence over a range some number of times.
playPattern2 :: Blink1 b => b -> (PatternStep, PatternStep) -> Word8 -> IO ()
playPattern2 b (sp, ep) n = command b 'p' [1, pos sp, pos ep, n]

-- | query the current play state.
getPlaying2 :: Blink1 b => b -> IO (Maybe (PatternStep, PatternStep, Word8, Word8))
getPlaying2 b = do
  _:a:sp:ep:n:i:_ <- request b 'S' []
  return $ if a > 0 then Just (PatternStep sp, PatternStep ep, n, i) else Nothing

-- | set the sequence pattern for the given position
setPattern :: Blink1 b => b -> PatternStep -> Delay -> RGB8 -> IO ()
setPattern b p d c = command b 'P' $ rgb c ++ delay d ++ [pos p]

getPattern :: Blink1 b => b -> PatternStep -> IO (Delay, RGB8)
getPattern dev p = do
  _:r:g:b:d1:d2:_ <- request dev 'R' $ rgb black ++ delay 0 ++ [pos p]
  return (fi (i d1 `shiftL` 8 .|. i d2) / 100, RGB r g b)
  where i = fi :: Word8 -> Word16

savePatterns2 :: Blink1 b => b -> IO ()
savePatterns2 b = command b 'W' [0xBE,0xEF,0xCA,0xFE]

eeaddr :: EEPROMAddr -> Word8
eeaddr = fi . fromEnum

readEEPROM :: Blink1 b => b -> EEPROMAddr -> IO Word8
readEEPROM b a = do
  _:_:v:_ <- request b 'e' [eeaddr a]
  return v

writeEEPROM :: Blink1 b => b -> EEPROMAddr -> Word8 -> IO ()
writeEEPROM b a v = command b 'E' [eeaddr a, v]

-- | This is only supported on mk1 devices.
getSerialNum :: Blink1 b => b -> IO Word32
getSerialNum b =
  foldl' (\l -> (l `shiftL` 8 .|.) . fi) 0 `liftM` 
    mapM (readEEPROM b . EESerialNum) [0..pred serialNumLen]

-- | This is only supported on mk1 devices.
setSerialNum :: Blink1 b => b -> Word32 -> IO ()
setSerialNum b s = mapM_ w [0..pred serialNumLen] where
  w i = writeEEPROM b (EESerialNum i) $ fi $ s `shiftR` (8*(3-fi i))

testBlink1 :: Blink1 b => b -> IO (Either [Word8] Bool)
testBlink1 b = do
  r <- request b '!' []
  return $ case r of
    0x55:0xAA:u:_ -> Right (u /= 0)
    _ -> Left r
