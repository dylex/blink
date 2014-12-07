module Loadavg
  ( startLoadavg
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (when)

import System.Hardware.Blink1.Types (RGB(..))

import Time
import Segment
import Activity
import Globals
import Blinker

data State = State
  { stateInterval :: Interval
  , stateColor :: Color
  } deriving (Eq)

update :: State -> Maybe Track -> Maybe Track
update (State l c) = Just . maybe (track s) (trackSwitch 2 s) where
  s = cycle [Segment 0 l c, Segment c l 0]

load :: IO Interval
load = li . rl <$> readFile "/proc/loadavg" where
  rl = read . (!! 1) . words
  li l = min 60 $ max 0.5 $ 4/l

loadavg :: Globals -> IO ()
loadavg globals = do
  key <- newKey (blinker1 globals)

  let change = updateAction key . update
      run s = do
        li <- load
        print li
        let s' = s{ stateInterval = li }
        when (s' /= s) $ change s'
        threadDelay 60
        run s'

  run $ State (1/0) (RGB 0 0.5 0.5)

startLoadavg :: Globals -> IO ThreadId
startLoadavg = forkIO . loadavg
