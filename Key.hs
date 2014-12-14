module Key
  ( Key
  , newKey
  ) where

import Control.Monad (join)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

type Key = Int

{-# NOINLINE keySource #-}
keySource :: IORef Int
keySource = unsafePerformIO (newIORef minBound)

newKey :: IO Key
newKey = atomicModifyIORef' keySource (join (,) . succ)
