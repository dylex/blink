{-# LANGUAGE MultiParamTypeClasses #-}
-- | Minimal interface to hidraw ioctls, sufficient for blink(1)
module System.Linux.HIDRaw
  ( DevInfo(..)
  , devInfo
  , setFeature
  , getFeature
  ) where

import Data.Int (Int16)
import Data.Word (Word8, Word32)
import Foreign.C.Error (errnoToIOError, eOPNOTSUPP)
import Foreign.C.Types (CInt)
import Foreign.Storable
import System.IO.Error (ioError)
import System.Posix.Types (Fd)
import System.Posix.IOCtl

#include <sys/ioctl.h>
#include <linux/hidraw.h>

data DevInfo = DevInfo 
  { devBustype :: Word32
  , devVendor :: Int16
  , devProduct :: Int16
  }

instance Storable DevInfo where
  sizeOf _ = #size struct hidraw_devinfo
  alignment _ = 4 -- #alignment struct hidraw_devinfo
  peek p = do
    b <- #{peek struct hidraw_devinfo, bustype} p
    v <- #{peek struct hidraw_devinfo, vendor} p
    i <- #{peek struct hidraw_devinfo, product} p
    return $ DevInfo b v i
  poke p (DevInfo b v i) = do
    #{poke struct hidraw_devinfo, bustype} p b
    #{poke struct hidraw_devinfo, vendor} p v
    #{poke struct hidraw_devinfo, product} p i

data HIDIOCGRAWINFO = HIDIOCGRAWINFO
instance IOControl HIDIOCGRAWINFO DevInfo where
  ioctlReq _ = #const HIDIOCGRAWINFO

devInfo :: Fd -> IO DevInfo
devInfo d = ioctl' d HIDIOCGRAWINFO

-- the ioctl package doesn't support these, so they're unimplemented for now
setFeature :: Fd -> [Word8] -> IO ()
setFeature d _ = ioError $ errnoToIOError "System.Linux.HIDRaw.setFeature" eOPNOTSUPP Nothing Nothing

getFeature :: Fd -> Int -> IO [Word8]
getFeature d _ = ioError $ errnoToIOError "System.Linux.HIDRaw.getFeature" eOPNOTSUPP Nothing Nothing
