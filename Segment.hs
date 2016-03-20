{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Segment
  ( Color
  , RGB(..)
  , fromRGB, toRGB
  , Segment(..)
  , solid
  , trunc
  , interp
  , Segment1(..)
  , fromSegment1, toSegment1
  ) where

import Control.Arrow (first)
import Control.Monad (liftM, liftM3)
import Data.Binary (Binary(..), putWord8, getWord8)
import Data.Typeable (Typeable)

import System.Hardware.Blink1.Types (RGB(..), black, RGB8, Delay)

import Time

type Color = RGB Float

instance Bounded Color where
  minBound = 0
  maxBound = 1

toRGB :: (Bounded a, Integral a) => Color -> RGB a
toRGB = fmap f where
  f x
    | x < 0 = 0
    | x > 1 = mx
    | otherwise = round (fromIntegral mx * x)
  mx = maxBound

fromRGB :: (Bounded a, Integral a) => RGB a -> Color
fromRGB = fmap f where
  f x = fi x / fi maxBound
  fi = fromIntegral

instance Read Color where
  readsPrec p = map (first fromRGB) . (readsPrec p :: ReadS RGB8)

data Segment = Segment
  { segColor :: !Color
  , segInterval :: !Interval
  , segEnd :: !Color
  } deriving (Show, Typeable)

solid :: Color -> Segment
solid c = Segment c (1/0) c

interp :: Segment -> Interval -> Color
interp (Segment s l e) r
  | r <= 0 = s
  | r >= l = e
  | otherwise = s + fmap (r / l *) (e - s)

instance Shiftable Segment where
  shift t f@(Segment _ l e)
    | l > t = Segment (interp f t) (l - t) e
    | otherwise = mempty

trunc :: Interval -> Segment -> Segment
trunc t f@(Segment s l _)
  | l > t = Segment s t (interp f t)
  | otherwise = f

instance Monoid Segment where
  mempty = solid black
  mappend f1@(Segment _ l1 _) f2@(Segment _ l2 _) = Segment (s1 + s2) l (e1 + e2) where
    l = min l1 l2
    Segment s1 _ e1 = trunc l f1
    Segment s2 _ e2 = trunc l f2

instance Binary Segment where
  put (Segment s l e) = put s >> put l >> put e
  get = liftM3 Segment get get get

data Segment1
  = Segment1Solid
    { seg1Color :: !RGB8 }
  | Segment1Fade
    { seg1Color :: !RGB8
    , seg1Delay :: !Delay
    , seg1End :: !RGB8
    } deriving (Eq, Show)

toSegment1 :: Segment -> Segment1
toSegment1 (Segment s l e)
  | isInfinite l = Segment1Solid s8
  | l <= maxDelay = Segment1Fade s8 (toDelay l) e8
  | otherwise = error "segment1 delay out of range"
  where
    s8 = toRGB s
    e8 = toRGB e

fromSegment1 :: Segment1 -> Segment
fromSegment1 (Segment1Solid c) = solid (fromRGB c)
fromSegment1 (Segment1Fade s l e) = Segment (fromRGB s) (fromDelay l) (fromRGB e)

instance Binary Segment1 where
  put (Segment1Solid c) = putWord8 0 >> put c
  put (Segment1Fade s l e) = putWord8 1 >> put s >> put l >> put e
  get = getWord8 >>= g where
    g 0 = liftM Segment1Solid get
    g 1 = liftM3 Segment1Fade get get get
    g _ = fail "invalid Segment1 binary tag"
