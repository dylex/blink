{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
{-|
  This a more general version of Data.Fixed that allows any Integral as the underlying representation.  It is very much like Data.Fixed.Binary (from fixed-point), but is not binary.
-}

module Data.Fixed.Prec
  ( FixedPrec(..) 
  , fixedToPrec
  , fixedFromPrec
  ) where

import Control.Arrow ((***), first)
import Data.Fixed
import Data.Typeable (Typeable)

newtype FixedPrec i a = MkFixedPrec i deriving (Eq, Ord, Enum, Bounded, Typeable)

precResolution :: (HasResolution a, Integral i) => FixedPrec i a -> i
precResolution = fromInteger . resolution

fixedFromPrec :: Integral i => FixedPrec i a -> Fixed a
fixedFromPrec (MkFixedPrec i) = MkFixed (toInteger i)

fixedToPrec :: Integral i => Fixed a -> FixedPrec i a
fixedToPrec (MkFixed i) = MkFixedPrec (fromInteger i)

instance (Integral i, HasResolution a) => Num (FixedPrec i a) where
  MkFixedPrec x + MkFixedPrec y = MkFixedPrec (x + y)
  MkFixedPrec x - MkFixedPrec y = MkFixedPrec (x - y)
  MkFixedPrec x * MkFixedPrec y = p where p = MkFixedPrec (x * y `div` precResolution p) -- XXX: overflow
  negate (MkFixedPrec x) = MkFixedPrec (negate x)
  abs (MkFixedPrec x) = MkFixedPrec (abs x)
  signum p@(MkFixedPrec x) = MkFixedPrec (signum x * precResolution p)
  fromInteger i = p where p = MkFixedPrec (fromInteger i * precResolution p)
instance (Integral i, HasResolution a) => Real (FixedPrec i a) where
  toRational p@(MkFixedPrec x) = toRational x / fromInteger (resolution p)
instance (Integral i, HasResolution a) => Fractional (FixedPrec i a) where
  MkFixedPrec x / MkFixedPrec y = p where p = MkFixedPrec (x * precResolution p `div` y) -- XXX: overflow
  recip p@(MkFixedPrec x) = MkFixedPrec (r * r `div` x) where r = precResolution p
  fromRational r = p where p = MkFixedPrec (floor (r * toRational (resolution p)))
instance (Integral i, HasResolution a) => RealFrac (FixedPrec i a) where
  properFraction p@(MkFixedPrec x) = fromIntegral *** MkFixedPrec $ x `divMod` precResolution p
  truncate p@(MkFixedPrec x) = fromIntegral (x `quot` precResolution p)
  floor p@(MkFixedPrec x) = fromIntegral (x `div` precResolution p)
  round p@(MkFixedPrec x) = fromIntegral ((x + pred r `div` 2) `div` r) where r = precResolution p
  ceiling p@(MkFixedPrec x) = fromIntegral ((x + pred r) `div` r) where r = precResolution p

instance (Integral i, HasResolution a) => Show (FixedPrec i a) where
  showsPrec p = showsPrec p . fixedFromPrec
instance (Integral i, HasResolution a) => Read (FixedPrec i a) where
  readsPrec p = map (first fixedToPrec) . readsPrec p
