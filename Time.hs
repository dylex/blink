{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Time 
  ( Time
  , Interval
  , timeInterval
  , delay
  , maxDelay, minDelay
  , threadDelay
  , now
  , Shiftable(..)
  ) where

import Control.Applicative ((<$))
import qualified Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Data.Fixed (E6)
import Data.Fixed.Prec
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Traversable (Traversable)

import System.Hardware.Blink1.Types (Delay(..))

type Time = UTCTime
type Interval = Float

timeInterval :: Time -> Time -> Interval
timeInterval t0 t1 = realToFrac (t1 `diffUTCTime` t0)

interval :: Delay -> Interval
interval = realToFrac

maxDelay :: Interval
maxDelay = interval maxBound

realToBounded :: (RealFrac a, Bounded b, RealFrac b) => a -> b
realToBounded x
  | x > realToFrac mx = mx
  | x < realToFrac mn = mn
  | otherwise = realToFrac x
  where
    mx = maxBound
    mn = minBound

delay :: Interval -> Delay
delay = realToBounded

newtype ThreadDelay = ThreadDelay (FixedPrec Int E6) deriving (Eq, Ord, Enum, Num, Real, Fractional, RealFrac)

instance Bounded ThreadDelay where
  minBound = ThreadDelay (MkFixedPrec 1)
  maxBound = ThreadDelay maxBound

minDelay :: Interval
minDelay = realToFrac (minBound :: ThreadDelay)

threadDelay :: Interval -> IO Interval
threadDelay i
  | isInfinite i = do
    -- there must be a better way to do this (that won't possibly trigger BlockedIndefinitely)..
    takeMVar =<< newEmptyMVar
  | otherwise = realToFrac d <$ Control.Concurrent.threadDelay m where
    d@(ThreadDelay (MkFixedPrec m)) = realToBounded i

class Monad m => Timed m where
  now :: m Time

instance Timed IO where
  now = getCurrentTime

instance Timed ((->) Time) where
  now = id

class Shiftable a where
  shift :: Interval -> a -> a
instance (Shiftable a, Traversable f) => Shiftable (f a) where
  shift = fmap . shift
