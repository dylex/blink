module System.Hardware.Blink1.USB
  ( Blink1USB
  , openUSB
  , openUSBs
  , closeUSB
  , getSerialNumber
  ) where

import Control.Monad
import System.IO.Error (ioError, mkIOError, doesNotExistErrorType)
import System.USB
import Bindings.Libusb.Descriptors (c'LIBUSB_CLASS_PER_INTERFACE, c'LIBUSB_CLASS_HID)
import qualified Data.Vector as V
import qualified Data.ByteString as BS (pack, unpack)
import qualified Data.Text as Text (unpack)
import System.Hardware.Blink1.Types
import System.Hardware.Blink1.Class

newtype Blink1USB = Blink1USB DeviceHandle

findUSBDev :: Ctx -> IO (V.Vector Device)
findUSBDev ctx = V.filterM (return . f <=< getDeviceDesc) =<< getDevices ctx where
  f d = deviceVendorId d == blink1Vendor 
    && deviceProductId d == blink1Product 
    && deviceClass d == c'LIBUSB_CLASS_PER_INTERFACE 
    && deviceNumConfigs d == 1
    -- ideally we should check the interfaces, but blink(1) only has one

interface :: Num a => a
interface = 0

openDev :: Device -> IO Blink1USB
openDev d = do
  d <- openDevice d
  kda <- kernelDriverActive d interface
  when kda $ detachKernelDriver d interface
  claimInterface d interface
  return $ Blink1USB d

openUSB :: IO Blink1USB
openUSB = do
  ctx <- newCtx
  d <- findUSBDev ctx
  when (V.null d) $ ioError $ mkIOError doesNotExistErrorType "Blink1.openUSB" Nothing Nothing
  openDev (V.head d)

openUSBs :: IO [Blink1USB]
openUSBs = mapM openDev . V.toList =<< findUSBDev =<< newCtx

closeUSB :: Blink1USB -> IO ()
closeUSB (Blink1USB d) = do
  releaseInterface d interface
  -- should really reattach here
  closeDevice d

getSerialNumber :: Blink1USB -> IO String
getSerialNumber (Blink1USB d) = liftM Text.unpack $ maybe 
  (ioError $ mkIOError doesNotExistErrorType "Blink1USB.getSerialNumber" Nothing Nothing) 
  (\i -> getStrDescFirstLang d i 16) . deviceSerialNumberStrIx =<< getDeviceDesc (getDevice d)

writeUSB :: Blink1USB -> [Word8] -> IO ()
writeUSB (Blink1USB d) x = writeControlExact d Class ToInterface 0x09 0x301 interface (BS.pack x) 1000

readUSB :: Blink1USB -> Int -> IO [Word8]
readUSB (Blink1USB d) n = liftM (BS.unpack . fst) $ readControl d Class ToInterface 0x01 0x301 interface n 1000

instance Blink1 Blink1USB where
  writeBlink1 = writeUSB
  readBlink1 = readUSB
  closeBlink1 = closeUSB
