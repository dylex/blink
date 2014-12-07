module Util
  ( guard1
  ) where

import Control.Monad (MonadPlus, mzero)

guard1 :: MonadPlus m => (a -> Bool) -> a -> m a
guard1 g x -- = x <$ guard (g x)
  | g x = return x
  | otherwise = mzero

