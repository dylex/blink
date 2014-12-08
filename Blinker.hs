{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, RankNTypes #-}
module Blinker
  ( Key
  , newKey
  , Blinker
  , startBlinker
  , updateAct
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (Exception, catch, throwTo)
import Control.Monad (ap)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat)
import Data.Typeable (Typeable, cast)
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Foldable (foldr)

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Types (LED)

import Util
import Time
import Segment
import Activity

newtype Blinker = Blinker { blinkerThread :: ThreadId }

data Key a = Key 
  { _keyId :: Unique
  , _keyThreadId :: ThreadId
  }

newKey :: Activity a => Blinker -> IO (Key a)
newKey = ap (Key <$> newUnique) . return . blinkerThread

data Update = forall a . Activity a => Update
  { _updateId :: Unique
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

type Acts = Map.Map Unique Act

updateActs :: Update -> Acts -> Acts
updateActs (Update k f) = Map.alter (fmap Act . up) k where
  up (Just (Act a)) = f $ cast a
  up Nothing = f Nothing

instance Show Update where
  show (Update k _) = "Update #" ++ (show (hashUnique k))

instance Exception Update

blinker :: Blink1 b => b -> Maybe LED -> Unmask -> IO ()
blinker b w unmask = run Map.empty where
  blnk = blink b w
  run acts = do
    t <- blnk $ mconcat $ map segment $ Map.elems acts
    t0 <- now
    (dt, u) <-
      ((t, Nothing) <$ unmask (threadDelay t)) `catch`
      (ap ((,) . timeInterval t0 <$> now) . return . Just)
    run $ Data.Foldable.foldr updateActs (Map.mapMaybe (guard1 active . shift dt) acts) u

startBlinker :: Blink1 b => b -> Maybe LED -> IO Blinker
startBlinker b = forkMasked Blinker . blinker b

updateAct :: Activity a => Key a -> (Maybe a -> Maybe a) -> IO ()
updateAct (Key k t) f = throwTo t (Update k f)
