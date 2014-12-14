{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes #-}
module Blinker
  ( ActKey
  , newActKey
  , Blinker
  , startBlinker
  , updateAct
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (Exception, catch, throwTo, finally)
import Control.Monad (ap)
import qualified Data.IntMap.Strict as Map
import Data.Monoid (mconcat)
import Data.Typeable (Typeable, cast)
import qualified Data.Foldable (foldr)

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Types (LED)

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

instance Show Update where
  show (Update k _) = "Update " ++ (show k)

instance Exception Update

blinker :: Blink1 b => IO () -> b -> Maybe LED -> Unmask -> IO ()
blinker done b w unmask = run Nothing Map.empty `finally` done where
  run cur acts = do
    let seg = mconcat $ map segment $ Map.elems acts
    t <- blink b w cur seg
    t0 <- now
    -- putStrLn (show seg ++ ", " ++ show t)
    (dt, u) <-
      ((t, Nothing) <$ unmask (threadDelay t)) `catch`
      (ap ((,) . timeInterval t0 <$> now) . return . Just)
    run (Just $ interp seg dt) $ Data.Foldable.foldr updateActs (Map.mapMaybe (guard1 active . shift dt) acts) u

startBlinker :: Blink1 b => IO () -> b -> Maybe LED -> IO Blinker
startBlinker done b = forkMasked Blinker . blinker done b

updateAct :: Activity a => ActKey a -> (Maybe a -> Maybe a) -> IO ()
updateAct (ActKey k t) f = throwTo t (Update k f)
