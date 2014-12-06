{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable, RankNTypes #-}
module Blink 
  ( blinker
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (Exception, catch)
import Control.Monad (ap)
import qualified Data.Map.Strict as Map
import Data.Monoid (mconcat)
import Data.Typeable (Typeable)
import Data.Unique (Unique, hashUnique)
import qualified Data.Foldable (foldr)

import Time
import Segment
import Activity

data Update = Update
  { updateKey :: Unique
  , updateAction :: Maybe Action -> Maybe Action
  } deriving (Typeable)

updateActions :: Update -> Map.Map Unique Action -> Map.Map Unique Action
updateActions (Update k a) = Map.alter a k

instance Show Update where
  show (Update k _) = "Update #" ++ (show (hashUnique k))

instance Exception Update

blinker :: (Segment -> IO Interval) -> (forall a . IO a -> IO a) -> IO ()
blinker blink unmask = run Map.empty where
  run acts = do
    t <- blink $ mconcat $ map actSegment $ Map.elems acts
    t0 <- now
    (dt, u) <-
      ((t, Nothing) <$ unmask (threadDelay t)) `catch`
      (ap ((,) . timeInterval t0 <$> now) . return . Just)
    run $ Data.Foldable.foldr updateActions (Map.mapMaybe (`actShift` dt) acts) u
