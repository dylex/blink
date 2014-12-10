module State
  ( State(..)
  , loadavgColor
  , decodeState
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Bits (finiteBitSize, setBit, unsafeShiftR, clearBit)
import Data.Monoid (Monoid(..))
import Data.Word (Word8)

import Util
import Segment

data State = State
  { stateMail :: !Int
  , statePurple :: !Int
  } deriving (Eq)

loadavgColor :: State -> Color
loadavgColor = color . stateMail where
  color 0 = RGB 0 0 0.5
  color n
    | n > 0 = RGB 0 0.5 0
    | otherwise = RGB 0 0.5 0.5

(|+|) :: Int -> Int -> Int
(|+|) (-1) x = x
(|+|) x (-1) = x
(|+|) x y = x + y

instance Monoid State where
  mempty = State (-1) (-1)
  mappend (State m1 p1) (State m2 p2) = State (m1 |+| m2) (p1 |+| p2)

mailBit :: Int
mailBit = pred $ finiteBitSize (0 :: Word8)

encodeState :: State -> Word8
encodeState (State m p) = guardEndo (m > 0) (`setBit` mailBit) (clearBit (fromIntegral (max 0 p)) mailBit)

decodeState :: Word8 -> State
decodeState w = State (fromIntegral $ unsafeShiftR w mailBit) (fromIntegral $ clearBit w mailBit)

instance Binary State where
  put = putWord8 . encodeState
  get = decodeState <$> getWord8
