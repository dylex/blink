{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.Foldable (mapM_)
import Data.List (foldl')
import Network.Socket (PortNumber(..))
import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as Opt

import System.Hardware.Blink1.Types (LED(..))
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (openDummy)
import System.Hardware.Blink1.Linux (openRawHID, openRawDev)
import System.Hardware.Blink1 (closeBlink1, getVersion)

import Blinker
import Loadavg
import Mail
import Pinger
import Purple
import Connect

data Blink1Dev = forall b . Blink1 b => Blink1Dev b

data Options = Options
  { optDevice :: IO Blink1Dev
  , optLEDs :: Maybe Int
  , optListen :: Maybe PortNumber
  , optConnect :: Maybe PortNumber
  } 

defaultOptions :: Options
defaultOptions = Options
  { optDevice = Blink1Dev <$> openRawHID
  , optLEDs = Nothing
  , optListen = Nothing -- Just 16652
  , optConnect = Nothing -- Just 6652
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "b" ["blink1"]
      (Opt.ReqArg (\f o -> o{ optDevice = Blink1Dev <$> openRawDev f }) "DEV")
      "use blink(1) hidraw device [auto]"
  , Opt.Option "n" ["dummy"]
      (Opt.NoArg (\o -> o{ optDevice = return (Blink1Dev (openDummy True)) }))
      "use dummy (debugging) blink(1) device"
  , Opt.Option "L" ["leds"]
      (Opt.ReqArg (\l o -> o{ optLEDs = Just (read l) }) "COUNT")
      "set the number of addressable LEDs"
  , Opt.Option "l" ["listen"]
      (Opt.OptArg (\p o -> o{ optListen = fmap (PortNum . read) p }) "PORT")
      ("(don't) listen on localhost:PORT serving events" ++ maybe "" ((" [" ++) . (++ "]") . show) (optListen defaultOptions))
  , Opt.Option "c" ["connect"]
      (Opt.OptArg (\p o -> o{ optConnect = fmap (PortNum . read) p }) "PORT")
      ("(don't) connect to localhost:PORT for events" ++ maybe "" ((" [" ++) . (++ "]") . show) (optConnect defaultOptions))
  ]

usage :: String
usage = Opt.usageInfo "Usage: blinkd [OPTION...]" options

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

  leds <-
    let
      vl ('1',_) = Nothing
      vl ('2',_) = Just (LED 2)
      vl (x,y) = error ("unknown blink(1) version: " ++ [x,'.',y] ++ " (maybe use -L)")
      ol 0 = Nothing
      ol n | n >= 1 && n <= 2 = Just (toEnum n)
      ol n = error ("unsupported LED count: " ++ show n)
    in maybe (vl <$> getVersion b) (return . ol) $ optLEDs opts

  bs <- mapM (startBlinker b) (maybe [Nothing] (map Just . enumFromTo minBound) leds)
  
  let led1:_:_ = cycle bs

  loadavg <- startLoadavg led1
  startMail loadavg
  startPinger led1
  purple <- initPurple led1
  Data.Foldable.mapM_ (startConnect purple) (optConnect opts)

  _ <- getLine
  return ()
