{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Hardware.Blink1.Types
  ( Word8
  , RGB(..)
  , black
  , Delay(..)
  , Pos(..)
  , EEPROMAddr(..)
  , serialNumLen
  ) where

import Data.Word (Word8)
import Data.Fixed (Centi)
import Numeric (showHex)

data RGB = RGB { red, green, blue :: !Word8 }

black :: RGB
black = RGB 0 0 0

showHex2 :: Word8 -> ShowS
showHex2 x
  | x < 16 = showChar '0' . showHex x
  | otherwise = showHex x

instance Show RGB where
  showsPrec _ (RGB r g b) = showChar '#' . showHex2 r . showHex2 g . showHex2 b

-- | time is counted in centiseconds
newtype Delay = Delay { delayCentiseconds :: Centi } deriving (Eq, Ord, Num, Real, Fractional, RealFrac)

instance Bounded Delay where
  minBound = Delay 0
  maxBound = Delay 655.36

instance Show Delay where
  showsPrec p (Delay s) = showsPrec p s . showChar 's'

-- | positions are counted 0-11
newtype Pos = Pos Word8 deriving (Eq, Ord, Enum, Num)

instance Bounded Pos where
  minBound = Pos 0
  maxBound = Pos 11

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

data BootMode
  = BootNormal
  | BootNightLight
  deriving (Eq, Ord, Enum, Bounded)
