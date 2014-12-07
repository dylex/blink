{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, RankNTypes, ExistentialQuantification, ImpredicativeTypes #-}
module Blinker
  ( newKey
  , updateAction
  , startBlinker
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Concurrent (ThreadId, forkIOWithUnmask, threadDelay)
import Control.Exception (Exception, catch, throwTo)
import Control.Monad (ap)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat)
import Data.Typeable (Typeable, cast)
import Data.Unique (Unique, newUnique, hashUnique)
import qualified Data.Foldable (foldr)

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Types (LED)

import Time
import Segment
import Activity

data Key a = Key { keyId :: Unique }

newKey :: Activity a => IO (Key a)
newKey = Key <$> newUnique

data Update = forall a . Activity a => Update
  { _updateKey :: Key a
  , _updateAct :: Maybe a -> Maybe a
  } deriving (Typeable)

updateAction :: Activity a => Key a -> (Maybe a -> Maybe a) -> ThreadId -> IO ()
updateAction k f t = throwTo t (Update k f)

data SomeActivity = forall a . Activity a => SomeActivity a deriving (Typeable)

instance Shiftable SomeActivity where
  shift t (SomeActivity a) = SomeActivity (shift t a)
instance Activity SomeActivity where
  actSegment (SomeActivity a) = actSegment a

type Activities = Map.Map Unique SomeActivity

updateActions :: Update -> Activities -> Activities
updateActions (Update (Key k) f) = Map.alter (fmap SomeActivity . up) k where
  up (Just (SomeActivity a)) = f $ cast a
  up Nothing = f Nothing

instance Show Update where
  show (Update k _) = "Update #" ++ (show (hashUnique (keyId k)))

instance Exception Update

blinker :: Blink1 b => b -> Maybe LED -> (forall a . IO a -> IO a) -> IO ()
blinker b w unmask = run Map.empty where
  blnk = blink b w
  run acts = do
    t <- blnk $ mconcat $ map actSegment $ Map.elems acts
    let d = delayMicroseconds t
    t0 <- now
    (dt, u) <-
      ((t, Nothing) <$ unmask (threadDelay d)) `catch`
      (ap ((,) . timeInterval t0 <$> now) . return . Just)
    run $ Data.Foldable.foldr updateActions (Map.map (shift dt) acts) u

startBlinker :: Blink1 b => b -> Maybe LED -> IO ThreadId
startBlinker b w = forkIOWithUnmask (blinker b w)
