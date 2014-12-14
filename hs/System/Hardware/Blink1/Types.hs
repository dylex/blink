{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module System.Hardware.Blink1.Types
  ( RGB(..), RGB8
  , black
  , Delay(..)
  , delayCentiseconds
  , PatternStep(..)
  , EEPROMAddr(..)
  , serialNumLen
  , LED(..)
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Monad (liftM3)
import Data.Binary (Binary(..))
import Data.Fixed (E2, Centi)
import Data.Fixed.Prec
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word (Word8, Word16)
import Numeric (showHex, readHex)

data RGB a = RGB { red, green, blue :: !a } deriving (Eq, Bounded, Typeable)

instance (Show a, Num a) => Show (RGB a) where
  showsPrec p (RGB r g b) = showParen (p > 10) $
    showString "RGB "
      . showsPrec 11 r . showChar ' '
      . showsPrec 11 g . showChar ' '
      . showsPrec 11 b

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

black :: Num a => RGB a
black = 0

clipAdd :: (Num a, Ord a, Bounded a) => a -> a -> a
clipAdd x y 
  | y > 0 && z < x = maxBound
  | y < 0 && z > x = minBound
  | z > maxBound = maxBound
  | z < minBound = minBound
  | otherwise = z
  where z = x + y

-- |like 'Sum' but clips overflowing values at 'maxBound'
instance (Num a, Ord a, Bounded a) => Monoid (RGB a) where
  mempty = 0
  mappend = liftA2 clipAdd

type RGB8 = RGB Word8

showHex2 :: Word8 -> ShowS
showHex2 x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x

-- |uses #RRGGBB format
instance Show RGB8 where
  showsPrec _ (RGB r g b) = showChar '#' . showHex2 r . showHex2 g . showHex2 b
instance Read RGB8 where
  readsPrec _ ('#':c) = rc2 c ++ rc1 c where
    rc1 (r:g:b:s) = rc [r,r] [g,g] [b,b] s
    rc1 _ = []
    rc2 (r1:r2:g1:g2:b1:b2:s) = rc [r1,r2] [g1,g2] [b1,b2] s
    rc2 _ = []
    rc r g b s = do
      (r,"") <- readHex r
      (g,"") <- readHex g
      (b,"") <- readHex b
      return (RGB r g b, s)
  readsPrec _ _ = []

instance Binary a => Binary (RGB a) where
  put (RGB r g b) = put r >> put g >> put b
  get = liftM3 RGB get get get

-- | time is measured in centiseconds
newtype Delay = Delay (FixedPrec Word16 E2) deriving (Bounded, Eq, Ord, Enum, Num, Real, Fractional, RealFrac, Binary, Typeable)

delayCentiseconds :: Delay -> Word16
delayCentiseconds (Delay (MkFixedPrec i)) = i

instance Show Delay where
  showsPrec p d = showsPrec p d . showChar 's'
instance Read Delay where
  readsPrec p = map f . readsPrec p where
    f (x,'s':s) = (realToFrac x, s)
    f (x,'c':'s':s) = (Delay (MkFixedPrec (floor x)), s)
    f (x,'m':'s':s) = (Delay (MkFixedPrec (floor x `div` 10)), s)
    f (x,s) = (realToFrac (x :: Centi), s)


-- | positions are counted 0-11 on mk1, 0-31 on mk2
newtype PatternStep = PatternStep { patternStep :: Word8 } deriving (Eq, Ord, Enum, Num, Show, Read, Binary, Typeable)

instance Bounded PatternStep where
  minBound = PatternStep 0
  maxBound = PatternStep 31 -- 11 on mk1

data EEPROMAddr
  = EEOSCCAL
  | EEBootMode
  | EESerialNum !Word8
  | EEPatternStart
  deriving (Eq, Ord, Typeable)

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

-- | LEDs are 1-based (0 means "all")
newtype LED = LED { whichLED :: Word8 } deriving (Eq, Ord, Enum, Num, Show, Read, Binary, Typeable)

instance Bounded LED where
  minBound = LED 1
  maxBound = LED maxBound
