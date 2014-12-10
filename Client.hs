module Client
  ( startClient
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void, forever, when)
import qualified Data.ByteString as BS
import Data.Monoid (mempty)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as Net.BS
import System.IO (hPutStrLn, stderr)
import System.IO.Error (mkIOError, eofErrorType, catchIOError)

import Time
import State
import Server

connect :: Server -> Net.SockAddr -> IO ()
connect server addr =
  bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \sock -> do
    Net.connect sock addr
    -- Net.shutdown sock Net.ShutdownSend
    let run o = do
          r <- Net.BS.recv sock 60
          when (BS.null r) $ ioError $ mkIOError eofErrorType "recv" Nothing Nothing
          let s = decodeState (BS.last r)
          print s
          updateServer server (stateUpdate o s)
          run s
    run mempty

client :: Server -> Net.PortNumber -> IO ()
client server port = do
  localhost <- Net.inet_addr "127.0.0.1" -- INADDR_LOOPBACK
  let addr = Net.SockAddrInet port localhost

  forever $ do
    connect server addr `catchIOError` \e -> hPutStrLn stderr ("client: " ++ show e)
    threadDelay 300

startClient :: Server -> Net.PortNumber -> IO ()
startClient server port = void $ forkIO $ client server port
