module Purple
  ( Purple
  , initPurple
  , updatePurple
  ) where

import Data.Bits (bit, testBit, clearBit)
import Data.List (intersperse)
import Data.Word (Word8)

import Time
import Segment
import State
import Activity
import Blinker

hue :: Float -> Color
hue h = case x :: Word8 of
  0 -> RGB 1 y 0
  1 -> RGB z 1 0
  2 -> RGB 0 1 y
  3 -> RGB 0 z 1
  4 -> RGB y 0 1
  5 -> RGB 1 0 z
  _ -> RGB 1 1 1
  where
    (x, y) = properFraction (6*h)
    z = 1 - y

color :: Word8 -> Color
color w = b * hue (fromIntegral (clearBit w highBit) / fromIntegral (pred (bit highBit) :: Word8)) where
  b = if testBit w highBit then 0.5 else 0.25

duration :: Interval
duration = 0.5

flash :: Color -> Segment
flash c = Segment c duration c

activity :: [Word8] -> Maybe Sequence
activity [] = Nothing
activity p = Just $ Sequence $ cycle $
  intersperse (flash 0) (map (flash . color) p) ++ [Segment 0 (3*duration) 0]

newtype Purple = Purple { _purpleKey :: ActKey Sequence }

initPurple :: Blinker -> IO Purple
initPurple blinker = Purple <$> newActKey blinker

updatePurple :: Purple -> [Word8] -> IO ()
updatePurple (Purple k) n = updateAct k (const $ activity n)
