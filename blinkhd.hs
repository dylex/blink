{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative ((<$>))
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.Foldable (mapM_)
import Data.List (foldl')
import Network.Socket (PortNumber)
import qualified System.Console.GetOpt as Opt
import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)

import System.Hardware.Blink1.Types (black, LED(..))
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (openDummy)
import System.Hardware.Blink1.Linux (openRawHID, openRawDev)
import System.Hardware.Blink1 (closeBlink1, getVersion, setColor)

import Blinker
import Loadavg
import Mail
import Pinger
import Purple
import Command
import Server
import Client

data Blink1Dev = forall b . Blink1 b => Blink1Dev b

data Options = Options
  { optDevice :: IO Blink1Dev
  , optLEDs :: Maybe Int
  , optListen :: Maybe PortNumber
  , optConnect :: Maybe PortNumber
  , optCommand :: Maybe FilePath
  } 

defaultOptions :: Options
defaultOptions = Options
  { optDevice = Blink1Dev <$> openRawHID
  , optLEDs = Nothing
  , optListen = Just 16652
  , optConnect = Just 6652
  , optCommand = Just "/tmp/.blink.ctl"
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "b" ["blink1"]
      (Opt.ReqArg (\f o -> o{ optDevice = Blink1Dev <$> openRawDev f }) "DEV")
      "use blink(1) hidraw device [auto]"
  , Opt.Option "n" ["dummy"]
      (Opt.NoArg (\o -> o{ optDevice = return (Blink1Dev (openDummy True)), optListen = Nothing, optConnect = Nothing, optCommand = Nothing }))
      "use dummy (debugging) blink(1) device (implies -L -C -S)"
  , Opt.Option "l" ["leds"]
      (Opt.ReqArg (\l o -> o{ optLEDs = Just (read l) }) "COUNT")
      "set the number of addressable LEDs"
  , Opt.Option "L" ["listen"]
      (Opt.OptArg (\p o -> o{ optListen = fmap (toEnum . read) p }) "PORT")
      ("(don't) listen on localhost:PORT serving events" ++ def (fmap show . optListen))
  , Opt.Option "C" ["connect"]
      (Opt.OptArg (\p o -> o{ optConnect = fmap (toEnum . read) p }) "PORT")
      ("(don't) connect to localhost:PORT for events" ++ def (fmap show . optConnect))
  , Opt.Option "S" ["socket"]
      (Opt.OptArg (\s o -> o{ optCommand = s }) "PATH")
      ("(don't) listen on socket PATH for commands" ++ def optCommand)
  ] where
  def f = maybe "" ((" [" ++) . (++ "]")) (f defaultOptions)

usage :: String
usage = Opt.usageInfo "Usage: blinkhd [OPTION...]" options

main :: IO ()
main = do
  args <- getArgs
  opts <- case Opt.getOpt Opt.Permute options args of
    (o, [], []) -> return $ foldl' (flip ($)) defaultOptions o
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure

  setCurrentDirectory =<< getHomeDirectory
  unless (optConnect opts == Nothing || optConnect opts /= optListen opts) $
    fail "cannot listen and connect on same port"

  bracket (optDevice opts) (\(Blink1Dev b) -> closeBlink1 b) $ \(Blink1Dev b) -> do
  setColor b black

  leds <-
    let
      vl ('1',_) = Nothing
      vl ('2',_) = Just (LED 2)
      vl (x,y) = error ("unknown blink(1) version: " ++ [x,'.',y] ++ " (maybe use -L)")
      ol 0 = Nothing
      ol n | n >= 1 && n <= 2 = Just (toEnum n)
      ol n = error ("unsupported LED count: " ++ show n)
    in maybe (vl <$> getVersion b) (return . ol) $ optLEDs opts

  wait <- newEmptyMVar
  let done = putMVar wait ()

  bs <- mapM (startBlinker done b) (maybe [Nothing] (map Just . enumFromTo minBound) leds)
  
  let led1:led2:_ = cycle bs

  loadavg <- startLoadavg led1
  purple <- initPurple led2
  server <- startServer loadavg purple (optListen opts)
  startMail server
  startPinger led2
  Data.Foldable.mapM_ (startCommand bs) (optCommand opts)
  Data.Foldable.mapM_ (startClient server) (optConnect opts)

  takeMVar wait
