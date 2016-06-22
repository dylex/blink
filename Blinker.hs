{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}
module Blinker
  ( ActKey
  , newActKey
  , Blinker
  , startBlinker
  , updateAct
  ) where

import Control.Exception (Exception(..), catch, throwTo, finally, asyncExceptionToException, asyncExceptionFromException)
import Control.Monad (ap, when)
import qualified Data.IntMap.Strict as Map
import Data.Typeable (Typeable, cast)

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Types (black, LED, Delay, Fade(..))
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
blinker done b w unmask = run (solid black) Map.empty `finally` done where
  run cur acts = do
    let seg@(Segment s t e) = mconcat $ map segment $ Map.elems acts
        seg'@(Segment _ l e') = trunc maxDelay seg
        s8 = toRGB s :: RGB8
        e8 = toRGB e :: RGB8
        e8' = toRGB e' :: RGB8
    t0 <- now
    -- print seg
    (c, st) <- case w of
      _ | s8 == toRGB (segColor cur) -> return (cur, 0)
      Nothing -> (solid s, 0) <$ setColor2 b w s8
      _ | l >= zeroDelay -> fadeToColor2 b w (Fade s8 0) >> (,) (solid s) <$> threadDelay zeroDelay
      _ -> return (cur, 0)
    (tt, next) <- if t == segInterval c && e8 == toRGB (segEnd c)
      then return (t, c)
      else do
        let Segment _ cl ce' = trunc l c
        when (cl /= l || e8' /= toRGB ce') $ fadeToColor2 b w (Fade e8' (toDelay (l-st)))
        return (l, seg')
    catch
      (unmask (threadDelay (tt-st)) >> run next (shiftActs tt acts))
      (\u -> do
        dt <- timeInterval t0 <$> now
        run (shift dt seg) $ updateActs u (shiftActs dt acts))

startBlinker :: Blink1 b => IO () -> b -> Maybe LED -> IO Blinker
startBlinker done b = forkMasked Blinker . blinker done b

updateAct :: Activity a => ActKey a -> (Maybe a -> Maybe a) -> IO ()
updateAct (ActKey k t) f = throwTo t (Update k f)
