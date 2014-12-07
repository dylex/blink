{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}
module Loadavg
  ( startLoadavg
  , setColor
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Concurrent (forkIOWithUnmask, ThreadId)
import Control.Exception (Exception, throwTo, catch)
import Control.Monad (when)
import Data.Typeable (Typeable)

import System.Hardware.Blink1.Types (RGB(..))

import Time
import Segment
import Activity
import Globals
import Blinker

data State = State
  { stateColor :: !Color
  , stateInterval :: !Interval
  } deriving (Eq)

update :: State -> Maybe Track -> Maybe Track
update (State c l) = Just . maybe (track s) (trackSwitch 2 s) where
  s = cycle [Segment 0 l c, Segment c l 0]

load :: IO Interval
load = li . rl <$> readFile "/proc/loadavg" where
  rl = read . (!! 1) . words
  li l = min 60 $ max 0.5 $ 4/l

newtype SetColor = SetColor Color deriving (Typeable, Show)
instance Exception SetColor

loadavg :: Globals -> (forall a . IO a -> IO a) -> IO ()
loadavg globals unmask = do
  key <- newKey (blinker1 globals)

  let change = updateAction key . update
      run d s = do
        when d $ change s
        s' <- (s <$ unmask (threadDelay 60)) `catch`
          (\(SetColor c) -> return s{ stateColor = c })
        li <- load
        let s'' = s'{ stateInterval = li }
        run (s'' /= s) s''

  run True =<< State (RGB 0 0.5 0.5) <$> load

newtype Loadavg = Loadavg { loadavgThread :: ThreadId }

startLoadavg :: Globals -> IO Loadavg
startLoadavg g = Loadavg <$> forkIOWithUnmask (loadavg g)

setColor :: Loadavg -> Color -> IO ()
setColor (Loadavg t) = throwTo t . SetColor
