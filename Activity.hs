{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveDataTypeable #-}
module Activity
  ( Activity(..)
  , Solid(..)
  , Sequence(..)
  , Track(..)
  , track
  , trackSwitch
  ) where

import qualified Data.Foldable (foldMap, any)
import Data.Monoid
import Data.Traversable (Traversable)
import Data.Typeable (Typeable, Typeable1)

import Time
import Segment

class (Shiftable a, Typeable a) => Activity a where
  segment :: a -> Segment
  active :: a -> Bool
  active _ = True

instance (Activity a, Traversable f, Typeable1 f) => Activity (f a) where
  segment = Data.Foldable.foldMap segment
  active = Data.Foldable.any active

newtype Solid = Solid Color deriving (Typeable)

instance Shiftable Solid where
  shift _ = id
instance Activity Solid where
  segment (Solid c) = solid c

newtype Sequence = Sequence { unSequence :: [Segment] } deriving (Typeable)

instance Shiftable Sequence where
  shift t (Sequence (f@(Segment _ l _) : r))
    | t >= l = shift (t - l) (Sequence r)
    | otherwise = Sequence $ shift t f : r
  shift _ s = s
instance Activity Sequence where
  segment (Sequence []) = mempty
  segment (Sequence (x:_)) = x
  active = not . null . unSequence

data Track = Track
  { trackSequence :: [Segment]
  , trackOffset :: Int
  , trackShift :: Interval
  } deriving (Typeable)

instance Shiftable Track where
  shift t (Track [] o s) = Track [] o (s+t)
  shift t (Track (f@(Segment _ l _) : r) o s)
    | t >= l = shift (t - l) (Track r (succ o) 0)
    | otherwise = Track (shift t f : r) o (s + t)
instance Activity Track where
  segment = segment . Sequence . trackSequence
  active = active . Sequence . trackSequence

track :: Sequence -> Track
track (Sequence s) = Track s 0 0

trackSwitch :: Int -> Sequence -> Track -> Track
trackSwitch n (Sequence l) (Track (Segment _ t0 _:_) o s0) | f : r <- drop o' l =
  let s' = s0 / (t0 + s0) * segInterval f in Track (shift s' f : r) o' s' where o' = o `mod` n
trackSwitch _ l _ = track l
