module State
  ( State(..)
  , loadavgColor
  , highBit
  , decodeState
  ) where

import Control.Applicative ((<$>))
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Bits
import Data.Monoid
import Data.Word (Word8)

import Segment

data State = State
  { stateMail :: !Int
  , statePurple :: [Word8]
  , statePending :: !Bool
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

instance Monoid State where
  mempty = State (-1) [] False
  mappend (State m1 p1 d1) (State m2 p2 d2) = State (m1 |+| m2) (p1 ++ p2) (d1 || d2)

highBit :: Int
highBit = 7 -- pred $ finiteBitSize (0 :: Word8)

consPurple :: Word8 -> State -> State
consPurple i s = s{ statePurple = i : statePurple s }

term :: Word8 -> Bool
term i = clearBit i highBit == 0

mail :: Word8 -> Int
mail i = fromEnum $ testBit i highBit

decodeState :: State -> Word8 -> State
decodeState s@State{ statePending = True } i
  | term i = s
    { stateMail = mail i
    , statePurple = statePurple s
    , statePending = False }
  | otherwise = consPurple i s
decodeState s i
  | term i = s
    { stateMail = mail i
    , statePurple = []
    }
  | otherwise = s
    { statePurple = [i]
    , statePending = True }

instance Binary State where
  put (State m p _) = do
    mapM_ putWord8 p
    putWord8 (if m > 0 then bit highBit else 0)
  get = do
    i <- getWord8 
    if term i
      then return $ State (mail i) [] False
      else consPurple i <$> get
