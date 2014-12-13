module State
  ( State(..)
  , loadavgColor
  , stateUpdate
  , decodeState
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Bits
import Data.Monoid
import Data.Word (Word8)

import Util
import Segment

data State = State
  { stateMail :: !Int
  , statePurple :: !Int
  } deriving (Eq, Show)

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

(|-|) :: Int -> Int -> Int
(|-|) (-1) (-1) = -1
(|-|) (-1) _ = error "-1 |-| x"
(|-|) x (-1) = x
(|-|) x y = x - y

instance Monoid State where
  mempty = State (-1) (-1)
  mappend (State m1 p1) (State m2 p2) = State (m1 |+| m2) (p1 |+| p2)

stateRemove :: State -> State -> State
stateRemove (State m0 p0) (State m1 p1) = State (m1 |-| m0) (p1 |-| p0)

stateUpdate :: State -> State -> State -> State
stateUpdate old new cur = stateRemove old cur <> new

mailBit :: Int
mailBit = 7 -- pred $ finiteBitSize (0 :: Word8)

encodeState :: State -> Word8
encodeState (State m p) = guardEndo (m > 0) (`setBit` mailBit) (clearBit (fromIntegral (max 0 p)) mailBit)

decodeState :: Word8 -> State
decodeState w = State (fromIntegral $ unsafeShiftR w mailBit) (fromIntegral $ clearBit w mailBit)

instance Binary State where
  put = putWord8 . encodeState
  get = decodeState <$> getWord8
