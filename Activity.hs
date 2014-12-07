{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Activity
  ( Activity(..)
  , Solid(..)
  , Sequence
  , Track(..)
  , track
  , trackSwitch
  ) where

import Data.Monoid
import Data.Typeable (Typeable)

import Time
import Segment

class (Shiftable a, Typeable a) => Activity a where
  segment :: a -> Segment
  active :: a -> Bool
  active _ = True

newtype Solid = Solid Color deriving (Typeable)

instance Shiftable Solid where
  shift _ = id
instance Activity Solid where
  segment (Solid c) = solid c

type Sequence = [Segment]

instance Shiftable Sequence where
  shift t (f@(Segment _ l _) : r)
    | t >= l = shift (t - l) r
    | otherwise = shift t f : r
  shift _ [] = []
instance Activity Sequence where
  segment [] = mempty
  segment (x:_) = x
  active = not . null

data Track = Track
  { trackSequence :: Sequence
  , trackOffset :: Int
  , trackShift :: Interval
  } deriving (Typeable)

instance Shiftable Track where
  shift t (Track [] o s) = Track [] o (s+t)
  shift t (Track (f@(Segment _ l _) : r) o s)
    | t >= l = shift (t - l) (Track r (succ o) 0) 
    | otherwise = Track (shift t f : r) o (s + t)
instance Activity Track where
  segment = segment . trackSequence
  active = active . trackSequence

track :: Sequence -> Track
track s = Track s 0 0

trackSwitch :: Int -> Sequence -> Track -> Track
trackSwitch n l (Track (Segment _ t0 _:_) o s0) | f : r <- drop o' l =
  let s' = s0 / (t0 + s0) * segInterval f in Track (shift s' f : r) o' s' where o' = o `mod` n
trackSwitch _ l _ = track l
