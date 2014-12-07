{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Activity
  ( Activity(..)
  , Solid(..)
  , Sequence
  , Track(..)
  , trackNorm
  , trackSwitch
  ) where

import Data.Monoid
import Data.Typeable (Typeable)

import Util
import Time
import Segment

class (Shiftable a, Typeable a) => Activity a where
  actSegment :: a -> Segment
  actShift :: Interval -> a -> Maybe a
  actShift = (Just .) . shift

newtype Solid = Solid Color deriving (Typeable)

instance Shiftable Solid where
  shift _ = id
instance Activity Solid where
  actSegment (Solid c) = solid c

type Sequence = [Segment]

instance Shiftable Sequence where
  shift t (f@(Segment _ l _) : r)
    | t >= l = shift (t - l) r
    | otherwise = shift t f : r
  shift _ [] = []
instance Activity Sequence where
  actSegment [] = mempty
  actSegment (x:_) = x
  actShift = (guard1 (not . null) .) . shift

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
  actSegment = actSegment . trackSequence
  actShift = (guard1 (not . null . trackSequence) .) . shift

trackNorm :: Int -> Track -> Track
trackNorm l t = t{ trackOffset = trackOffset t `mod` l }

trackSwitch :: Sequence -> Track -> Track
trackSwitch l (Track _ o s) = Track (drop o l) o s
