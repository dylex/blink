module System.Hardware.Blink1.Dummy
  ( Blink1Dummy
  , openDummy
  ) where

import System.Hardware.Blink1.Class

data Blink1Dummy = Blink1Dummy
  { _verbose :: Bool
  }

openDummy :: Bool -> Blink1Dummy
openDummy = Blink1Dummy

instance Blink1 Blink1Dummy where
  writeBlink1 (Blink1Dummy False) _ = return ()
  writeBlink1 (Blink1Dummy True) l = print l
  readBlink1 _ n = return (replicate n 0)
  closeBlink1 _ = return ()
