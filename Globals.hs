module Globals
  ( Globals(..)
  ) where

import System.Hardware.Blink1.Types (LED)

import Blinker (Blinker)

data Globals = Globals
  { maxLEDs :: Maybe LED
  , blinker1, blinker2 :: Blinker
  }
