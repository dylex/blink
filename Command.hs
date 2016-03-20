module Command
  ( Command(..)
  , startCommand
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (liftM2, liftM3, forever, void, unless)
import Data.Binary (Binary(..), decodeOrFail)
import qualified Data.ByteString.Lazy as BS (null)
import qualified Data.Foldable (mapM_)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as Net.BS
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (removeLink, setFileCreationMask)
import System.Posix.Types (CMode(..))
import System.IO.Error (catchIOError, isDoesNotExistErrorType, ioeGetErrorType)
import System.IO.Unsafe (unsafeInterleaveIO)

import System.Hardware.Blink1.Types (LED)

import Segment
import Activity
import Blinker

data Command 
  = CmdSequence
    { cmdWhich :: LED
    , cmdRepeat :: Int
    , cmdSequence :: [Segment1]
    }

instance Binary Command where
  put (CmdSequence w n s) = put w >> put n >> put s
  get = liftM3 CmdSequence get get get

loop :: Int -> [a] -> [a]
loop _ [] = []
loop 0 _ = []
loop n l
  | n < 0 = cycle l
  | otherwise = l ++ loop (pred n) l

activity :: Command -> Sequence
activity (CmdSequence _ n s) = Sequence $ loop n $ map fromSegment1 s

mapUnsafeInterleaveIO :: (a -> IO b) -> [a] -> IO [b]
mapUnsafeInterleaveIO _ [] = return []
mapUnsafeInterleaveIO f (x:l) = unsafeInterleaveIO $ 
  liftM2 (:) (f x) $ mapUnsafeInterleaveIO f l

server :: [Blinker] -> FilePath -> IO ()
server leds path =
  bracket (Net.socket Net.AF_UNIX Net.Datagram Net.defaultProtocol) Net.close $ \sock -> do
    ks <- mapUnsafeInterleaveIO newActKey leds
    removeLink path `catchIOError` \e ->
      unless (isDoesNotExistErrorType (ioeGetErrorType e)) (ioError e)
    bracket (setFileCreationMask (CMode 0o177)) setFileCreationMask $ \_ ->
      Net.bind sock addr
    let 
      err = hPutStrLn stderr . ("command: " ++)
      key = k ks where
        k (x:l) n
          | n > 1 = k l (pred n)
          | n >= 0 = return $ Just x
        k _ _ = Nothing <$ err "invalid LED"
      command cmd = do
        mk <- key (fromEnum $ cmdWhich cmd)
        Data.Foldable.mapM_ (\k -> updateAct k (const $ Just act)) mk
        where act = activity cmd
      msg (Left (_, _, e)) = err e
      msg (Right (r, _, c))
        | BS.null r = command c
        | otherwise = err "command: excess data"
    forever $ do
      buf <- Net.BS.recv sock 8192
      msg $ decodeOrFail buf
  where addr = Net.SockAddrUnix path

startCommand :: [Blinker] -> FilePath -> IO ()
startCommand leds path = void $ forkIO (server leds path)
