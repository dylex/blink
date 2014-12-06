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
import Data.Fixed (resolution, HasResolution(..))
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

toResolution :: (RealFrac a, HasResolution b, Bounded b, Enum b) => a -> b
toResolution x
  | x' > fromEnum mx = mx
  | x' < fromEnum mn = mn
  | otherwise = toEnum x'
  where
    x' = round (res * x)
    res = fromIntegral $ resolution (Just mx)
    mx = maxBound
    mn = minBound

delay :: Interval -> Delay
delay = toResolution

newtype ThreadDelay = ThreadDelay Int deriving (Eq, Ord, Enum)

instance HasResolution ThreadDelay where
  resolution _ = 1000000

instance Bounded ThreadDelay where
  minBound = ThreadDelay 0
  maxBound = ThreadDelay maxBound

threadDelay :: Interval -> IO ()
threadDelay i = Control.Concurrent.threadDelay d
  where ThreadDelay d = toResolution i

class Monad m => Timed m where
  now :: m Time

instance Timed IO where
  now = getCurrentTime

instance Timed ((->) Time) where
  now = id

class Shiftable a where
  shift :: a -> Interval -> a
