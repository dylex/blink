module Server
  ( Server
  , startServer
  , updateServer
  ) where

import Control.Concurrent (forkIO, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, tryTakeMVar, tryPutMVar, readMVar, modifyMVar_)
import Control.Exception (bracket)
import Control.Monad (void, forever, when)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (ByteString, empty)
import qualified Data.Foldable (mapM_)
import qualified Data.IntMap.Lazy as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty, mconcat)
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as Net.BS
import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)
import System.Posix.Types (Fd(Fd))

import Key
import State
import Loadavg
import Purple

data Server = Server
  { serverStates :: !(MVar (Map.IntMap State))
  , serverState :: !(IORef BS.ByteString)
  , serverTrigger :: !(MVar ())
  , serverLoadavg :: !Loadavg
  , serverPurple :: Purple
  }

serve :: Server -> Net.Socket -> IO ()
serve Server{ serverState = v, serverTrigger = t } sock = do
  Net.shutdown sock Net.ShutdownReceive
  let run = do
        threadWaitWrite (Fd (Net.fdSocket sock))
        _ <- tryTakeMVar t
        _ <- Net.BS.sendAll sock =<< readIORef v
        readMVar t
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
  m <- newMVar Map.empty
  v <- newIORef BS.empty
  r <- newEmptyMVar
  let s = Server
        { serverStates = m
        , serverState = v
        , serverTrigger = r
        , serverLoadavg = loadavg
        , serverPurple = purple
        }
  Data.Foldable.mapM_ (forkIO . server s) port
  return s

updateServer :: Server -> Key -> State -> IO ()
updateServer srv k v = modifyMVar_ (serverStates srv) $ \m -> do
  let (vo, m')
        | v == mempty = Map.updateLookupWithKey (\_ _ -> Nothing) k m
        | otherwise = Map.insertLookupWithKey (\_ _ _ -> v) k v m
      v' = fromMaybe mempty vo
      s = mconcat $ Map.elems m'
  when (v' /= v) $ do
    when (loadavgColor v' /= loadavgColor v) $
      setLoadavgColor (serverLoadavg srv) (loadavgColor s)
    when (statePurple v' /= statePurple v) $
      updatePurple (serverPurple srv) (statePurple s)
    writeIORef (serverState srv) (encode s) >>= void . tryPutMVar (serverTrigger srv)
  return m'
