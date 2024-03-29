{-|

  Blink(1) hardware interface using Linux's /dev/hidraw* devices.

-}

module System.Hardware.Blink1.Linux
  ( Blink1Raw
  , openRawDev
  , openRawHID
  , openRawHIDs
  ) where

import Control.Exception (Exception, catchJust, onException, bracket)
import Control.Monad
import Data.List (isPrefixOf, genericLength)
import Data.Word (Word8)
import Foreign.C.Error (errnoToIOError, eFTYPE) -- hack
import GHC.IO.Exception (IOErrorType(ProtocolError))
import Numeric (readHex)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (mkIOError, fullErrorType, doesNotExistErrorType, ioeGetErrorType)
import System.Posix.IO
import System.Posix.Directory (openDirStream, readDirStream, closeDirStream)
import System.Posix.Types (Fd)
import Foreign.Marshal.Array

import System.Linux.HIDRaw
import System.Hardware.Blink1.Class

newtype Blink1Raw = Blink1Raw Fd

-- | Open the given blink(1) hidraw device
openRawDev :: FilePath -> IO Blink1Raw
openRawDev f = do
  d <- openFd df ReadWrite Nothing defaultFileFlags
  i <- devInfo d `onException` closeFd d
  when (devVendor i /= blink1Vendor || devProduct i /= blink1Product) $ do
    closeFd d
    ioError $ errnoToIOError "not Blink1" eFTYPE Nothing (Just f)
  return $ Blink1Raw d
  where df = case f of { '/':_ -> f ; _ -> "/dev/" ++ f }

findRawDev :: MonadPlus m => IO (m String)
findRawDev = pds dp hiddir where
  hiddir = "/sys/bus/hid/devices"
  pds f d = bracket (openDirStream d) closeDirStream r where
    r ds = do
      e <- readDirStream ds
      if null e then return mzero else liftM2 mplus (f e) (r ds)
  dp f | null (do
      (_,':':vs) <- rh f
      (v,':':ps) <- rh vs
      guard (v == blink1Vendor)
      (p,'.':_) <- rh ps
      guard (p == blink1Product)) = return mzero
    | otherwise = pds fp (hiddir ++ '/' : f ++ "/hidraw")
  fp f = return $ guard ("hidraw" `isPrefixOf` f) >> return f
  rh = readHex

-- | Search for and open the first blink(1) hidraw device
openRawHID :: IO Blink1Raw
openRawHID = maybe 
  (ioError $ mkIOError doesNotExistErrorType "Blink1.openRawHID" Nothing Nothing) 
  openRawDev =<< findRawDev
  
-- | Search for and open all blink(1) hidraw devices
openRawHIDs :: IO [Blink1Raw]
openRawHIDs = mapM openRawDev =<< findRawDev

retryWhen :: Exception e => Int -> (e -> Maybe String) -> IO a -> IO a
retryWhen 0 _ f = f
retryWhen n g f = catchJust g f $ \e -> do
  hPutStrLn stderr (e ++ ". retrying...")
  retryWhen (pred n) g f

writeRaw :: Blink1Raw -> [Word8] -> IO ()
writeRaw (Blink1Raw d) x = do -- setFeature d x
  let l = genericLength x
  r <- withArray x $ \p -> retryWhen 1 (\e -> "fdWriteBuf: protocol error" <$ guard (ProtocolError == ioeGetErrorType e)) $ fdWriteBuf d p l
  when (r /= l) $ ioError $ mkIOError fullErrorType "Blink1Raw: short write" Nothing Nothing

readRaw :: Blink1Raw -> Int -> IO [Word8]
readRaw (Blink1Raw d) n = tail `liftM` getFeature d n
  
closeRaw :: Blink1Raw -> IO ()
closeRaw (Blink1Raw d) = closeFd d

instance Blink1 Blink1Raw where
  writeBlink1 = writeRaw
  readBlink1 = readRaw
  closeBlink1 = closeRaw
