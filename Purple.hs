module Purple
  ( Purple
  , initPurple
  , updatePurple
  ) where

import Control.Applicative ((<$>))
import Control.Monad (guard)
import Data.List (intersperse)

import Time
import Segment
import Activity
import Blinker

color :: Color
color = RGB 0.5 0.5 0

duration :: Interval
duration = 0.5

flash :: Color -> Segment
flash c = Segment c duration c

activity :: Int -> Maybe Sequence
activity n = do
  guard (n > 0)
  return $ Sequence $ cycle $
    intersperse (flash 0) (replicate n (flash color)) ++ [Segment 0 (3*duration) 0]

newtype Purple = Purple { _purpleKey :: Key Sequence }

initPurple :: Blinker -> IO Purple
initPurple blinker = Purple <$> newKey blinker

updatePurple :: Purple -> Int -> IO ()
updatePurple (Purple k) n = updateAct k (const $ activity n)
