module Server
  ( Server
  , startServer
  , updateServer
  ) where

import Control.Concurrent (forkIO, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, tryPutMVar, readMVar)
import Control.Exception (bracket)
import Control.Monad (void, forever, when)
import Data.Binary (encode)
import qualified Data.Foldable (mapM_)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Monoid (mempty)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as Net.BS
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Types (Fd(Fd))

import State
import Loadavg
import Purple

data Server = Server
  { serverState :: !(IORef State)
  , serverSync :: !(MVar ())
  , serverLoadavg :: !Loadavg
  , serverPurple :: Purple
  }

serve :: Server -> Net.Socket -> IO ()
serve Server{ serverState = v, serverSync = r } sock = do
  Net.shutdown sock Net.ShutdownReceive
  let run = do
        threadWaitWrite (Fd (Net.fdSocket sock))
        _ <- tryTakeMVar r
        _ <- Net.BS.sendAll sock . encode =<< readIORef v
        readMVar r
        run
  run

server :: Server -> Net.PortNumber -> IO ()
server s port = do
  localhost <- Net.inet_addr "127.0.0.1" -- INADDR_LOOPBACK
  let addr = Net.SockAddrInet port localhost
  bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \sock -> do
    Net.setSocketOption sock Net.ReuseAddr 1
    Net.bind sock addr
    Net.listen sock 1
    forever $ bracket (Net.accept sock) (Net.close . fst) $ \(c, _) ->
      serve s c `catchIOError` \e -> hPutStrLn stderr ("server: " ++ show e)

startServer :: Loadavg -> Purple -> Maybe Net.PortNumber -> IO Server
startServer loadavg purple port = do
  v <- newIORef mempty
  r <- newEmptyMVar
  let s = Server
        { serverState = v
        , serverSync  = r
        , serverLoadavg = loadavg
        , serverPurple = purple
        }
  Data.Foldable.mapM_ (forkIO . server s) port
  return s

updateServer :: Server -> (State -> State) -> IO ()
updateServer s@Server{ serverState = vr, serverSync = r } m = do
  (v', v) <- atomicModifyIORef' vr $ \v' ->
    let v = m v' in (v, (v', v))
  when (v' /= v) $ do
  let lc' = loadavgColor v'
      lc = loadavgColor v
      p' = statePurple v'
      p = statePurple v
  when (lc' /= lc) $ setLoadavgColor (serverLoadavg s) lc
  when (p' /= p) $ updatePurple (serverPurple s) p
  void $ tryPutMVar r ()
