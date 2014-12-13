{-# LANGUAGE DeriveDataTypeable #-}
module Keys
  ( Key
  , newKey
  , KeyMap
  , empty
  , clean
  , alter
  , mapMaybe
  , elems
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (join, (<=<))
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.IntMap.Strict as Map
import qualified Data.Maybe (mapMaybe)
import qualified Data.Traversable (mapM)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (Weak, mkWeakPair, deRefWeak)

data Key = Key { _unKey :: Int } deriving (Eq, Ord, Typeable, Show)

{-# NOINLINE keySource #-}
keySource :: IORef Int
keySource = unsafePerformIO (newIORef minBound)

newKey :: IO Key
newKey = Key <$> atomicModifyIORef' keySource (join (,) . succ)

type Elem a = Weak (Key, a)

value :: Elem a -> IO (Maybe a)
value e = fmap snd <$> deRefWeak e

mkElem :: Key -> a -> IO (Elem a)
mkElem k v = mkWeakPair k v Nothing

mkElems :: Key -> Maybe a -> IO (Maybe (Elem a))
mkElems = Data.Traversable.mapM . mkElem

newtype KeyMap a = KeyMap { _unkeyMap :: Map.IntMap (Elem a) }

empty :: KeyMap a
empty = KeyMap Map.empty

clean :: KeyMap a -> KeyMap a
clean (KeyMap m) = KeyMap $ Map.mapMaybe (unsafePerformIO . wf) m where
  wf w = (w <$) <$> deRefWeak w

alter :: (Maybe a -> Maybe a) -> Key -> KeyMap a -> KeyMap a
alter f k@(Key i) (KeyMap m) = KeyMap $ Map.alter (unsafePerformIO . wf) i m where
  wf Nothing = mf Nothing
  wf (Just e) = mf =<< value e
  mf = mkElems k . f

mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap $ Map.mapMaybe (unsafePerformIO . wf) m where
  wf = ef <=< deRefWeak
  ef Nothing = return Nothing
  ef (Just (k, v)) = mkElems k $ f v

elems :: KeyMap a -> [a]
elems (KeyMap m) = Data.Maybe.mapMaybe (unsafePerformIO . value) $ Map.elems m
