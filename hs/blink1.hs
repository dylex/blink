{-# LANGUAGE CPP, RankNTypes, ExistentialQuantification #-}

import Control.Applicative ((<$), (<|>))
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as Opt

import System.Hardware.Blink1
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (Blink1Dummy, openDummy)
#ifdef USE_LINUX
import System.Hardware.Blink1.Linux (openRawHID, openRawDev)
#endif
#ifdef USE_USB
import System.Hardware.Blink1.USB (openUSB)
#endif

data State = forall b . Blink1 b => State 
  { _dev :: Maybe b
  , led :: Maybe LED
  , time :: Delay
  }

noDev :: Maybe Blink1Dummy
noDev = Nothing

withDev :: State -> (forall b . Blink1 b => b -> a) -> a
withDev (State Nothing _ _) _ = error "No device opened"
withDev (State (Just d) _ _) f = f d

opening :: Blink1 b => IO b -> State -> IO State
opening o (State Nothing l t) = o >>= \b -> return (State (Just b) l t)
opening o s = close s >>= opening o

close :: State -> IO State
close (State (Just b) l t) = State noDev l t <$ closeBlink1 b
close s = return s

run :: (forall b . Blink1 b => b -> IO a) -> (a -> IO ()) -> State -> IO State
run f g s = withDev s f >>= g >> return s

options :: [Opt.OptDescr (State -> IO State)]
options =
  [ Opt.Option "D" ["dummy"]
      (Opt.NoArg $ opening (return $ openDummy True))
      "open a dummy blink(1) device"
#ifdef USE_LINUX
  , Opt.Option "L" ["hidraw"]
      (Opt.OptArg (\f -> opening (maybe openRawHID openRawDev f)) "DEVICE")
      "open a linux hidraw device"
#endif
#ifdef USE_USB
  , Opt.Option "U" ["usb"]
      (Opt.NoArg $ opening openUSB)
      "open a libusb device"
#endif
  , Opt.Option "V" ["version"]
      (Opt.NoArg $ run getVersion (\(a, b) -> putStrLn [a,'.',b]))
      "get the version number"
  , Opt.Option "T" ["test"]
      (Opt.NoArg $ run testBlink1 (putStrLn . either (\r -> "unexpected: " ++ show r) (\r -> if r then "pass" else "fail")))
      "issue the test command"
  , Opt.Option "S" ["serial"]
      (Opt.NoArg $ run getSerialNum print)
      "get the serial number (only works on mk1)"
  , Opt.Option "l" ["led"]
      (Opt.OptArg (\l s -> return s{ led = fmap (toEnum . read) l }) "INDEX")
      "apply the next actions only to LED INDEX [all]"
  , Opt.Option "g" ["get"]
      (Opt.OptArg (\l s -> do
        withDev s getColor2 (fromMaybe 0 (fmap (toEnum . read) l <|> led s)) >>= print
        return s) "LED")
      "get the current color of the LED"
  , Opt.Option "s" ["set"]
      (Opt.ReqArg (\c s -> do
        withDev s setColor2 (led s) (read c)
        return s) "RGB")
      "set the color immediately"
  , Opt.Option "t" ["time"]
      (Opt.ReqArg (\t s -> return s{ time = read t }) "DELAY")
      "set the delay/fade time for the next actions"
  , Opt.Option "f" ["fade"]
      (Opt.ReqArg (\c s -> do
        withDev s fadeToColor2 (led s) (time s) (read c)
        return s) "RGB")
      "fade to color over DELAY"
  ]

usage :: String
usage = Opt.usageInfo "Usage: blink1 [OPTION...]" options

main :: IO State
main = do
  args <- getArgs
  r <- case Opt.getOpt Opt.RequireOrder options args of
    (r, [], []) -> return r
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure
  foldM (flip ($)) (State noDev Nothing 0) r >>= close
