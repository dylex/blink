{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Segment
  ( Color, RGB(..)
  , Segment(..)
  , solid
  , interp
  , blink
  ) where

import Control.Arrow (first)
import Control.Monad (when)
import qualified Data.Foldable (all)
import Data.Monoid
import Data.Typeable (Typeable)

import System.Hardware.Blink1.Types (RGB(..), black, RGB8, LED)
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1 (setColor2, fadeToColor2)

import Time

type Color = RGB Float

instance Bounded Color where
  minBound = 0
  maxBound = 1

rgb :: (Bounded a, Integral a) => Color -> RGB a
rgb = fmap f where
  f x
    | x < 0 = 0
    | x > 1 = mx
    | otherwise = round (fromIntegral mx * x)
  mx = maxBound

color :: (Bounded a, Integral a) => RGB a -> Color
color = fmap f where
  f x = fi x / fi maxBound
  fi = fromIntegral

instance Read Color where
  readsPrec p = map (first color) . (readsPrec p :: ReadS RGB8)

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

blink :: Blink1 b => b -> Maybe LED -> Maybe Color -> Segment -> IO Interval
blink b w cur f@(Segment s l e) | l < 0 = fail ("invalid segment delay: " ++ show l)
  | otherwise = do
  let s8 = rgb s
  when (Data.Foldable.all ((/=) s8 . rgb) cur) $ setColor2 b w s8
  if isInfinite l || rgb e == s8
    then return l
    else do
      let Segment _ l' e' = trunc maxDelay f
          e8' = rgb e'
      when (e8' /= s8) $ fadeToColor2 b w (delay l') e8'
      return l'
