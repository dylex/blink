{-# LANGUAGE RankNTypes, DeriveDataTypeable #-}
module Loadavg
  ( startLoadavg
  , Loadavg
  , setLoadavgColor
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (Exception, throwTo, catch)
import Control.Monad (when)
import Data.Typeable (Typeable)
import System.IO (withFile, IOMode(ReadMode), hGetLine, hSeek, SeekMode(AbsoluteSeek))

import Util
import Time
import Segment
import Activity
import Blinker

data State = State
  { stateColor :: !Color
  , stateInterval :: !Interval
  } deriving (Eq)

update :: State -> Maybe Track -> Maybe Track
update (State c l) = Just . maybe (track s) (trackSwitch 2 s) where
  s = Sequence $ cycle [Segment 0 l c, Segment c l 0]
  
interval :: String -> Interval
interval l = min 60 $ max 0.5 $ 4 / read (words l !! 1)

newtype SetColor = SetColor Color deriving (Typeable, Show)
instance Exception SetColor

loadavg :: Blinker -> Unmask -> IO ()
loadavg blinker unmask = withFile "/proc/loadavg" ReadMode $ \h -> do
  key <- newKey blinker

  let load = do
        hSeek h AbsoluteSeek 0
        interval <$> hGetLine h
      change = updateAct key . update
      run d s = do
        when d $ change s
        s' <- (s <$ unmask (threadDelay 60)) `catch`
          (\(SetColor c) -> return s{ stateColor = c })
        li <- load
        let s'' = s'{ stateInterval = li }
        run (s'' /= s) s''

  run True =<< State (RGB 0 0.5 0.5) <$> load

newtype Loadavg = Loadavg { _loadavgThread :: ThreadId }

startLoadavg :: Blinker -> IO Loadavg
startLoadavg = forkMasked Loadavg . loadavg

setLoadavgColor :: Loadavg -> Color -> IO ()
setLoadavgColor (Loadavg t) = throwTo t . SetColor
