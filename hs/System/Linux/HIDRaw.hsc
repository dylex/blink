{-# LANGUAGE MultiParamTypeClasses, ForeignFunctionInterface #-}
-- | Minimal interface to hidraw ioctls, sufficient for blink(1)
module System.Linux.HIDRaw
  ( DevInfo(..)
  , devInfo
  , setFeature
  , getFeature
  ) where

import Data.Bits ((.|.), shiftL)
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array
import Foreign.Storable
import System.Posix.Types (Fd)
import System.Posix.IOCtl

#include <sys/ioctl.h>
#include <linux/hidraw.h>

data DevInfo = DevInfo 
  { devBustype :: Word32
  , devVendor :: Word16
  , devProduct :: Word16
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

-- the ioctl package doesn't support variable-length data, so we call them directly

foreign import ccall unsafe "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

ioctlLen :: Storable p => Fd -> CInt -> Int -> Ptr p -> IO ()
ioctlLen f r l p 
  | len > mask = ioError $ errnoToIOError "ioctlLen" eMSGSIZE Nothing Nothing
  | otherwise = throwErrnoIfMinus1_ "ioctl" $ c_ioctl (fromIntegral f) (r .|. (len `shiftL` shift)) (castPtr p)
  where 
    len = fromIntegral $ l * sizeOf (ptrType p)
    ptrType :: Ptr p -> p
    ptrType _ = undefined
    shift = #const _IOC_SIZESHIFT
    mask = #const _IOC_SIZEMASK

setFeature :: Fd -> [Word8] -> IO ()
setFeature d x = withArrayLen x $ ioctlLen d #{const HIDIOCSFEATURE(0)}

getFeature :: Fd -> Int -> IO [Word8]
getFeature d l = allocaArray l $ \p -> do
  ioctlLen d #{const HIDIOCGFEATURE(0)} l p
  peekArray l p
