module Connect
  ( startConnect
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import qualified Network.Socket as Net
import System.IO.Error (mkIOError, eofErrorType)

import Purple

connect :: Purple -> Net.PortNumber -> IO ()
connect purple port = do
  localhost <- Net.inet_addr "127.0.0.1" -- INADDR_LOOPBACK
  let addr = Net.SockAddrInet port localhost

  bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \sock -> do
    Net.connect sock addr
    Net.shutdown sock Net.ShutdownSend
    let run = got =<< Net.recv sock 1
        got [] = ioError $ mkIOError eofErrorType "connect: recv" Nothing Nothing
        got r = do
          mapM_ (updatePurple purple . fromEnum) r
          run
    run

startConnect :: Purple -> Net.PortNumber -> IO ()
startConnect purple port = void $ forkIO $ connect purple port
