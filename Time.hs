{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Time 
  ( Time
  , Interval
  , timeInterval
  , delay
  , maxDelay
  , threadDelay
  , now
  , Shiftable(..)
  ) where

import qualified Control.Concurrent (threadDelay)
import Data.Fixed (E6)
import Data.Fixed.Prec
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

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
  minBound = ThreadDelay 0
  maxBound = ThreadDelay maxBound

delayMicroseconds :: Interval -> Int
delayMicroseconds i = m where ThreadDelay (MkFixedPrec m) = realToBounded i

threadDelay :: Interval -> IO ()
threadDelay = Control.Concurrent.threadDelay . delayMicroseconds

class Monad m => Timed m where
  now :: m Time

instance Timed IO where
  now = getCurrentTime

instance Timed ((->) Time) where
  now = id

class Shiftable a where
  shift :: Interval -> a -> a
