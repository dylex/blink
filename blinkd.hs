{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.List (foldl')
import System.Directory (setCurrentDirectory, getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as Opt

import System.Hardware.Blink1.Types (LED(..))
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (openDummy)
import System.Hardware.Blink1.Linux (openRawHID, openRawDev)
import System.Hardware.Blink1 (closeBlink1, getVersion)

import Globals
import Blinker
import Loadavg
import Mail

data Blink1Dev = forall b . Blink1 b => Blink1Dev b

data Options = Options
  { optDevice :: IO Blink1Dev
  , optLEDs :: Maybe Int
  } 

defaultOptions :: Options
defaultOptions = Options
  { optDevice = Blink1Dev <$> openRawHID
  , optLEDs = Nothing
  }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "b" ["blink1"]
      (Opt.ReqArg (\f o -> o{ optDevice = Blink1Dev <$> openRawDev f }) "DEV")
      "use blink(1) hidraw device"
  , Opt.Option "n" ["dummy"]
      (Opt.NoArg (\o -> o{ optDevice = return (Blink1Dev (openDummy True)) }))
      "use dummy (debugging) blink(1) device"
  , Opt.Option "L" ["leds"]
      (Opt.ReqArg (\l o -> o{ optLEDs = Just (read l) }) "COUNT")
      "force the number of addressable LEDs"
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

  bracket (optDevice opts) (\(Blink1Dev b) -> closeBlink1 b) $ \(Blink1Dev b) -> do

  leds <-
    let
      vl ('1',_) = Nothing
      vl ('2',_) = Just (LED 2)
      vl (x,y) = error ("Unknown blink(1) version: " ++ [x,'.',y] ++ " (maybe use -L)")
      ol 0 = Nothing
      ol n | n >= 1 && n <= 2 = Just (toEnum n)
      ol n = error ("Unsupported LED count: " ++ show n)
    in maybe (vl <$> getVersion b) (return . ol) $ optLEDs opts

  bs <- mapM (startBlinker b) (maybe [Nothing] (map Just . enumFromTo minBound) leds)
  
  let globals = gf bs where 
        gf [b0] = Globals leds b0 b0
        gf ~[b1,b2] = Globals leds b1 b2

  loadavg <- startLoadavg globals
  startMail loadavg

  _ <- getLine
  return ()
