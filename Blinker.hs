{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Blinker
  ( ActKey
  , newActKey
  , Blinker
  , startBlinker
  , updateAct
  ) where

import Control.Exception (Exception(..), catch, throwTo, finally, asyncExceptionToException, asyncExceptionFromException)
import Control.Monad (ap)
import qualified Data.IntMap.Strict as Map
import Data.Typeable (Typeable, cast)

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Types (black, LED, Delay)
import System.Hardware.Blink1 (setColor2, fadeToColor2)

import Util
import Key
import Time
import Segment
import Activity

newtype Blinker = Blinker { blinkerThread :: ThreadId }

data ActKey a = ActKey 
  { _keyId :: Key
  , _keyThreadId :: ThreadId
  }

newActKey :: Activity a => Blinker -> IO (ActKey a)
newActKey = ap (ActKey <$> newKey) . return . blinkerThread

data Update = forall a . Activity a => Update
  { _updateId :: Key
  , _updateAct :: Maybe a -> Maybe a
  } deriving (Typeable)

data Act = forall a . Activity a => Act
  { _activity :: a
  } deriving (Typeable)

instance Shiftable Act where
  shift t (Act a) = Act (shift t a)
instance Activity Act where
  segment (Act a) = segment a
  active (Act a) = active a

type Acts = Map.IntMap Act

updateActs :: Update -> Acts -> Acts
updateActs (Update k f) = Map.alter (fmap Act . up) k where
  up (Just (Act a)) = f $ cast a
  up Nothing = f Nothing

shiftActs :: Interval -> Acts -> Acts
shiftActs dt = Map.mapMaybe (guard1 active . shift dt)

instance Show Update where
  show (Update k _) = "Update " ++ (show k)

instance Exception Update where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

zeroDelay :: Interval
zeroDelay = fromDelay (toEnum 1 :: Delay)

blinker :: Blink1 b => IO () -> b -> Maybe LED -> Unmask -> IO ()
blinker done b w unmask = run black Map.empty `finally` done where
  run cur acts = do
    let seg@(Segment s t e) = mconcat $ map segment $ Map.elems acts
        Segment _ l e' = trunc maxDelay seg
        s8 = toRGB s
        e8 = toRGB e
        e8' = toRGB e'
    t0 <- now
    -- print seg
    st <- case w of
      _ | s8 == cur -> return 0
      Nothing -> 0 <$ setColor2 b w s8
      _ | l > zeroDelay -> fadeToColor2 b w 0 s8 >> threadDelay zeroDelay
      _ -> return 0
    (lt, next) <- if s8 == e8 || isInfinite t then return (t-st, s8) else do
      wt <- if e8' /= s8
        then fadeToColor2 b w (toDelay (l-st)) e8' >> threadDelay zeroDelay
        else return 0
      return (l-st-wt, e8')
    catch
      (unmask (threadDelay lt) >> run next (shiftActs lt acts))
      (\u -> do
        dt <- timeInterval t0 <$> now
        run (toRGB $ interp seg dt) $ updateActs u (shiftActs dt acts))

startBlinker :: Blink1 b => IO () -> b -> Maybe LED -> IO Blinker
startBlinker done b = forkMasked Blinker . blinker done b

updateAct :: Activity a => ActKey a -> (Maybe a -> Maybe a) -> IO ()
updateAct (ActKey k t) f = throwTo t (Update k f)
