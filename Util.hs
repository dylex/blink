{-# LANGUAGE RankNTypes #-}
module Util
  ( guard1
  , guardEndo
  , forkMasked, Unmask, ThreadId
  ) where

import Control.Applicative (Alternative, pure, empty)
import Control.Concurrent (forkIOWithUnmask, ThreadId)
import Control.Exception (mask_)

guard1 :: Alternative f => (a -> Bool) -> a -> f a
guard1 g x -- = x <$ guard (g x)
  | g x = pure x
  | otherwise = empty

guardEndo :: Bool -> (a -> a) -> (a -> a)
guardEndo False _ = id
guardEndo True f = f

type Unmask = forall a . IO a -> IO a

forkMasked :: (ThreadId -> a) -> (Unmask -> IO ()) -> IO a
forkMasked w = fmap w . mask_ . forkIOWithUnmask
