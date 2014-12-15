
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (length)
import Data.List (foldl')
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as Net.BS
import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (mkIOError, fullErrorType)

import Command (Command(..))

data Options = Options
  { optSocket :: FilePath
  , optCommand :: Command
  }

defaultOptions :: Options
defaultOptions = Options
  { optSocket = "/tmp/.blink.ctl"
  , optCommand = CmdSequence 0 1 []
  }

optCommand' :: (Command -> Command) -> (Options -> Options)
optCommand' f o = o{ optCommand = f (optCommand o) }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "S" ["socket"]
      (Opt.ReqArg (\s o -> o{ optSocket = s }) "PATH")
      ("conenct to socket PATH for commands" ++ def (Just . optSocket))
  , Opt.Option "l" ["led"]
      (Opt.ReqArg (\l -> optCommand' (\c -> c{ cmdWhich = toEnum (read l) })) "INDEX")
      ("apply the commands to LED INDEX" ++ def (Just . show . fromEnum . cmdWhich . optCommand))
  ] where
  def f = maybe "" ((" [" ++) . (++ "]")) (f defaultOptions)

usage :: String
usage = Opt.usageInfo "Usage: blinkh [OPTION...]" options

main :: IO ()
main = do
  args <- getArgs
  opts <- case Opt.getOpt Opt.Permute options args of
    (o, [], []) -> return $ foldl' (flip ($)) defaultOptions o
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure

  bracket (Net.socket Net.AF_UNIX Net.Datagram Net.defaultProtocol) Net.close $ \sock -> do
  Net.connect sock (Net.SockAddrUnix (optSocket opts))

  let send cmd = do
        r <- Net.BS.send sock d
        unless (r == BS.length d) $ ioError $ mkIOError fullErrorType "send" Nothing (Just (optSocket opts))
        where d = encode cmd

  send (optCommand opts)

  return ()
