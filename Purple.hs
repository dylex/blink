module Purple
  ( Purple
  , initPurple
  , updatePurple
  ) where

import Data.Bits (bit, testBit)
import Data.List (intersperse)
import Data.Word (Word8)

import Time
import Segment
import State
import Activity
import Blinker

frac :: RealFrac a => a -> a
frac = (snd :: (Integer, a) -> a) . properFraction

hue :: Float -> Color
hue h
  | x <= 1 = RGB 1 y 0
  | x <= 2 = RGB z 1 0
  | x <= 3 = RGB 0 1 y
  | x <= 4 = RGB 0 z 1
  | x <= 5 = RGB y 0 1
  | otherwise = RGB 1 0 z
  where
    x = 6*frac h
    y = frac x
    z = 1 - y

color :: Word8 -> Color
color w = b * hue (fromIntegral w / fromIntegral (pred (bit highBit) :: Word8)) where
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
