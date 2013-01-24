module System.Hardware.Blink1.Class
  ( Blink1(..)
  , Word8
  , blink1Vendor, blink1Product
  ) where

import Data.Word (Word8, Word16)

-- we could parameterize on monad too, if it comes to that
class Blink1 b where
  writeBlink1 :: b -> [Word8] -> IO ()
  readBlink1 :: b -> Int -> IO [Word8]
  closeBlink1 :: b -> IO ()

blink1Vendor, blink1Product :: Word16
blink1Vendor = 0x27B8
blink1Product = 0x1ED
