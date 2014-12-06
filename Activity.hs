{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Activity
  ( Action(..)
  , Activity(..)
  , Sequence
  ) where

import Data.Monoid

import Time
import Segment

data Action = Action
  { actSegment :: Segment
  , actShift :: Interval -> Maybe Action
  }

class Shiftable a => Activity a where
  activeSegment :: a -> Segment

instance Activity Color where
  activity c = a where
    a = Just $ Action (solid c) (const a)

type Sequence = [Segment]

current :: Sequence -> Segment
current [] = mempty
current (x:_) = x

instance Shiftable Sequence where
  shift [] _ = []
  shift (f@(Segment _ l _) : r) t
    | t >= l = shift r (t - l)
    | otherwise = shift f t : r

instance Activity Sequence where
  activity [] = Nothing
  activity l = Just $ Action (current l) (activity . shift l)


data Track = Track
  { trackSequence :: Sequence
  , trackOffset :: Int
  , trackShift :: Interval
  }

instance Shiftable Track where
  shift (Track [] o s) t = Track [] o (s+t)
  shift (Track (f@(Segment _ l _) : r) o s) t
    | t >= l = shift (Track r (succ o) 0) (t - l)
    | otherwise = Track (shift f t : r) o (s + t)
