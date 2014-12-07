{-# LANGUAGE ExistentialQuantification #-}

import Control.Applicative ((<$>))
import Control.Concurrent (ThreadId)
import Control.Exception (mask_)
import Data.List (foldl')
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as Opt

import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (openDummy)
import System.Hardware.Blink1.Linux (openRawHID, openRawDev)
import System.Hardware.Blink1 (getVersion)

import Blinker

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
  , Opt.Option "D" ["dummy"]
      (Opt.NoArg (\o -> o{ optDevice = return (Blink1Dev (openDummy True)) }))
      "use dummy (debugging) blink(1) device"
  , Opt.Option "L" ["leds"]
      (Opt.ReqArg (\l o -> o{ optLEDs = Just (read l) }) "COUNT")
      "force the number of addressable LEDs"
  ]

usage :: String
usage = Opt.usageInfo "Usage: blinkd [OPTION...]" options

data Globals = Globals
  { blinker1, blinker2 :: ThreadId
  }

main :: IO ()
main = do
  args <- getArgs
  opts <- case Opt.getOpt Opt.Permute options args of
    (o, [], []) -> return $ foldl' (flip ($)) defaultOptions o
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure

  Blink1Dev b <- optDevice opts

  leds <- let
    lf ('1',_) = 0
    lf ('2',_) = 2
    lf (x,y) = error ("Unknown blink(1) version: " ++ [x,'.',y] ++ " (maybe use -L)")
    in maybe (lf <$> getVersion b) return $ optLEDs opts

  bs <- mask_ $ mapM (startBlinker b) $ case leds of
    0 -> [Nothing]
    n | n >= 1 && n <= 2 -> map Just (enumFromTo minBound (toEnum n))
    n -> error ("Unexpected LED count: " ++ show n)
  
  let globals = gf bs where 
        gf [b0] = Globals b0 b0
        gf ~[b1,b2] = Globals b1 b2

  _ <- getLine
  return ()
