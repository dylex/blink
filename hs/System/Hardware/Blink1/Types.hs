{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module System.Hardware.Blink1.Types
  ( RGB(..), RGB8
  , black
  , Delay(..)
  , second
  , PatternStep(..)
  , EEPROMAddr(..)
  , serialNumLen
  ) where

import Control.Applicative
import Control.Arrow ((***))
import Data.Fixed (HasResolution(..), Centi)
import Data.Monoid (Monoid(..))
import Data.Word (Word8, Word16)
import Numeric (showHex, readHex)

data RGB a = RGB { red, green, blue :: !a }
type RGB8 = RGB Word8

black :: RGB8
black = RGB 0 0 0

instance Functor RGB where
  fmap f (RGB r g b) = RGB (f r) (f g) (f b)
instance Applicative RGB where
  pure x = RGB x x x
  RGB fr fg fb <*> RGB r g b = RGB (fr r) (fg g) (fb b)

instance Num a => Num (RGB a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

clipAdd :: (Num a, Ord a, Bounded a) => a -> a -> a
clipAdd x y 
  | z < x = maxBound
  | otherwise = z
  where z = x + y

-- |like 'Sum' but clips overflowing values at 'maxBound'
instance (Num a, Ord a, Bounded a) => Monoid (RGB a) where
  mempty = 0
  mappend = liftA2 clipAdd

showHex2 :: Word8 -> ShowS
showHex2 x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x

-- |uses #RRGGBB format
instance Show RGB8 where
  showsPrec _ (RGB r g b) = showChar '#' . showHex2 r . showHex2 g . showHex2 b
instance Read RGB8 where
  readsPrec _ ('#':c) = rc2 c ++ rc1 c where
    rc1 (r:g:b:s) = rc (0x11*) [r] [g] [b] s
    rc1 _ = []
    rc2 (r1:r2:g1:g2:b1:b2:s) = rc id [r1,r2] [g1,g2] [b1,b2] s
    rc2 _ = []
    rc f r g b s = do
      (r,"") <- readHex r
      (g,"") <- readHex g
      (b,"") <- readHex b
      return (RGB (f r) (f g) (f b), s)
  readsPrec _ _ = []

-- | time is measured in centiseconds
newtype Delay = Delay { delayCentiseconds :: Word16 } deriving (Bounded, Eq, Ord, Enum)

sec :: Num a => a
sec = 100

second :: Delay
second = Delay sec

-- This boiler-plate fixed-point is all possibly over-kill, but is hopefully at least unambiguous, and better than using Centi
instance HasResolution Delay where
  resolution _ = sec

-- | operations are based on seconds
instance Num Delay where
  Delay x + Delay y = Delay (x + y)
  Delay x - Delay y = Delay (x - y)
  Delay x * Delay y = Delay (x * y `div` sec) -- XXX: overflow
  negate (Delay x) = Delay (negate x)
  abs (Delay x) = Delay (abs x)
  signum (Delay x) = Delay (signum x * sec)
  fromInteger i = Delay (fromInteger i * sec)
instance Real Delay where
  toRational (Delay x) = toRational x / sec
instance Fractional Delay where
  Delay x / Delay y = Delay (x * sec `div` y) -- XXX: overflow
  recip (Delay x) = Delay (sec * sec `div` x)
  fromRational r = Delay (floor (r * sec))
instance RealFrac Delay where
  properFraction (Delay x) = fromIntegral *** Delay $ x `divMod` sec
  truncate (Delay x) = fromIntegral (x `div` sec)
  round (Delay x) = truncate (Delay (x + (pred sec `div` 2)))
  ceiling (Delay x) = truncate (Delay (x + pred sec))
  floor x = truncate x -- unsigned!

instance Show Delay where
  showsPrec p d = showsPrec p (realToFrac d :: Centi) . showChar 's'
instance Read Delay where
  readsPrec p = map f . readsPrec p where
    f (x,'s':s) = (realToFrac x, s)
    f (x,'c':'s':s) = (Delay (floor x), s)
    f (x,'m':'s':s) = (Delay (floor x `div` 10), s)
    f (x,s) = (realToFrac (x ::
#if MIN_VERSION_base(4,4,0)
        Centi
#else
        Float
#endif
      ), s)


-- | positions are counted 0-11
newtype PatternStep = PatternStep Word8 deriving (Eq, Ord, Enum, Num, Show, Read)

instance Bounded PatternStep where
  minBound = PatternStep 0
  maxBound = PatternStep 11

data EEPROMAddr
  = EEOSCCAL
  | EEBootMode
  | EESerialNum Word8
  | EEPatternStart
  deriving (Eq, Ord)

serialNumLen :: Word8
serialNumLen = 4

instance Enum EEPROMAddr where
  fromEnum EEOSCCAL = 0
  fromEnum EEBootMode = 1
  fromEnum (EESerialNum i) 
    | i < serialNumLen = 2 + fromIntegral i
    | otherwise = error "EEPROMAddr.fromEnum: invalid EESerialNum"
  fromEnum EEPatternStart = 6
  toEnum 0 = EEOSCCAL
  toEnum 1 = EEBootMode
  toEnum 6 = EEPatternStart
  toEnum x 
    | x >= 2 && x < 6 = EESerialNum (fromIntegral x-2)
    | otherwise = error "EEPROMAddr.toEnum: invalid address"

instance Bounded EEPROMAddr where
  minBound = EEOSCCAL
  maxBound = EEPatternStart
