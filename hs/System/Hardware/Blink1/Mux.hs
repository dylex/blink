{-# LANGUAGE CPP #-}
-- | This module allows multiple blink(1) actions (from different threads/users/sources/timescales) to be scheduled concurrently, without having to touch the EEPROM patterns.  Overlapping actions have their colors added (using the 'Monoid RGB8' instance which prevents overflow).
module System.Hardware.Blink1.Mux
  ( Blink1Mux
  , startBlink1Mux
  , stopBlink1Mux
  ) where

import Control.Concurrent
import System.Hardware.Blink1

#ifdef VERSION_time
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)

type Time = UTCTime
getTime = getCurrentTime
deltaTime s e = round $ 100 * diffUTCTime s e

#else
import System.Time (ClockTime(..), getClockTime) 

type Time = ClockTime
getTime = getClockTime
deltaTime (TOD ss sp) (TOD es ep) = fromIntegral $ (es - ss) * 100 + (ep - sp + 4999999999) `div` 10000000000

#endif

getTime :: IO Time
deltaTime :: Time -> Time -> Int

clipIntegral :: (Integral a, Integral b, Bounded b) => a -> b
clipIntegral x
  | x > fromIntegral mx = mx
  | x < fromIntegral mn = mn
  | otherwise = fromIntegral x
  where
    mn = minBound
    mx = maxBound

type Blink1Handle = Unique

data Segment
  = SegmentColor
    { segColor :: !RGB8 }
  | SegmentFade
    { segColor :: !RGB8
    , segDelay :: !Delay
    , segEnd :: !RGB8
    }

data Activity = Activity
  { actHandle :: !Blink1Handle
  , actWait :: !Int
  , actSegment :: !Segment
  }

data State = State
  { stateAct :: [Activity]
  , stateLast :: !Time
  }

data Blink1Mux = Blink1Mux
  { muxThread :: ThreadId
  , muxActive :: MVar State
  }

data Wakeup = Wakeup deriving (Typeable, Show, Exception)

interpSeg :: Bool -> Delay -> Segment -> RGB8
interpSeg _ _ (SegmentColor c) = c
interpSeg dir t (SegmentFade s d e)
  | t <= 0 = x
  | t >= d = y
  | otherwise = x + fmap (round . (f *)) (fmap fromIntegral y - fmap fromIntegral x)
  where 
    f = realToFrac t / realToFrac d :: Float
    (x,y) = if dir then (s,e) else (e,s)

instance Monoid Segment where
  mempty = SegmentColor 0
  mappend (SegmentColor c1) (SegmentColor c2) = SegmentColor (mappend c1 c2)
  mappend (SegmentColor c) (SegmentFade s d e) = SegmentFade (mappend c s) d (mappend c e)
  mappend (SegmentFade s d e) (SegmentColor c) = SegmentFade (mappend s c) d (mappend e c)
  mappend f1@(SegmentFade s1 d1 e1) f2@(SegmentFade s2 d2 e2) =
    uncurry (SegmentFade (mappend s1 s2)) $ case compare d1 d2 of
      LT -> (d1, mappend e1 (interpSeg True d1 f2))
      EQ -> (d1, mappend e1 e2)
      GT -> (d2, mappend (interpSeg True d2 f1) e2)

segDelay :: Segment -> Maybe Int
segDelay SegmentFade{ segDelay = d } = Just $ delayCentiseconds d
segDelay _ = Nothing

actDelay :: Activity -> Maybe Int
actDelay Activity{ actWait = d, actSegment = s }
  | d > 0 = d
  | otherwise = segLength s

actInc :: Int -> [Activity] -> [Activity]
actInc _ [] = []
actInc d (a:al) = a{ actWait = actWait a + d } : al

activityAdd :: Activity -> [Activity] -> [Activity]
activityAdd a [] = [a]
activityAdd a (a1:al)
  | actDelay a < actDelay a1 = a : a1{ actWait = actWait a1 - actDelay a } : al
  | otherwise = a1 : activityAdd a{ actWait = actWait a - actDelay a1 } al

activityRemove :: Blink1Handle -> [Activity] -> [Activity]
activityRemove _ [] = []
activityRemove h (a:al)
  | actHandle a == h = actInc (actDelay a) al
  | otherwise = a : activityRemove h al

actShift :: Int -> [Activity] -> [Activity]
actShift _ [] = []
actShift t (a:al)
  | actDelay a' <= 0 = actShift (t - actDelay a) al
  | otherwise = activityAdd a' (actInc (actDelay a') al)
  where a' = a{ actWait = actWait a - t }

trimSeg :: Delay -> Segment -> Segment

evalAct :: [Activity] -> Segment
evalAct [] = SegmentNone
evalAct (Activity{ actStarted = False }:al) = evalAct al
evalAct (Activity{ actWait = d, actSegment = s }:al) = s{ segColor = interpSeg False d s }

runSeg :: Blink1 b => b -> Segment -> IO ()
runSeg b SegmentNone = runSeg b (SegmentColor 0)
runSeg b s = setColor b (segColor s) >> fade s where
  fade SegmentFade{ segDelay = d, segEnd = c } = fadeToColor b d c
  fade _ = return ()

runMux :: Blink1 b => b -> MVar State -> (IO () -> IO ()) -> IO ()
runMux b sv unmask = run . State [] <<= getTime where run s = do
  putMVar sv s
  handle (\Wakeup -> return ()) $ unmask $ 
    threadDelay $ (*) 10000 $ fromIntegral $ case act of
      [] -> maxBound -- FIXME
      Activity{ actDiff = d }:_ -> d
  State act st <- takeMVar sv
  et <- getTime
  let dt = clipIntegral $ deltaTime et st
      act' = shiftAct dt act
      a = evalAct act'
  runSeg b a
  run $ State act' et

startBlink1Mux :: Blink1 b => b -> IO Blink1Mux
startBlink1Mux b = do
  sv <- newEmptyMVar
  th <- forkIOWithUnmask (runMux b sv)
  return $ Blink1Mux th sv

stopBlink1Mux :: Blink1Mux -> IO ()
stopBlink1Mux = killThread . muxThread
