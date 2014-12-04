{-# LANGUAGE CPP, RankNTypes #-}

import Control.Applicative ((<$>), (<|>))
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified System.Console.GetOpt as Opt

import System.Hardware.Blink1
import System.Hardware.Blink1.Class (Blink1)
import System.Hardware.Blink1.Dummy (Blink1Dummy, openDummy)
#ifdef USE_LINUX
import System.Hardware.Blink1.Linux (Blink1Raw, openRawHID, openRawDev)
#endif
#ifdef USE_USB
import System.Hardware.Blink1.USB (Blink1USB, openUSB)
#endif

data Dev
  = NoDev
  | DummyDev Blink1Dummy
#ifdef USE_LINUX
  | LinuxRawDev Blink1Raw
#endif
#ifdef USE_USB
  | USBDev Blink1USB
#endif

withDev :: Dev -> (forall b . Blink1 b => b -> a) -> a
withDev NoDev _ = error "No device opened"
withDev (DummyDev b) f = f b
#ifdef USE_LINUX
withDev (LinuxRawDev b) f = f b
#endif
#ifdef USE_USB
withDev (USBDev b) f = f b
#endif

withDev_ :: Dev -> (forall b . Blink1 b => b -> IO ()) -> IO ()
withDev_ NoDev _ = return ()
withDev_ d f = withDev d f

data State = State 
  { dev :: !Dev
  , led :: Maybe LED
  , time :: Delay
  }

opening :: IO Dev -> State -> IO State
opening o s = do
  withDev_ (dev s) closeBlink1
  d <- o
  return s{ dev = d }

close :: State -> IO State
close = opening (return NoDev)

run :: (forall b . Blink1 b => b -> IO a) -> (a -> IO ()) -> State -> IO State
run f g s = withDev (dev s) f >>= g >> return s

options :: [Opt.OptDescr (State -> IO State)]
options =
  [ Opt.Option "D" ["dummy"]
      (Opt.NoArg $ opening (return $ DummyDev $ openDummy True))
      "open a dummy blink(1) device"
#ifdef USE_LINUX
  , Opt.Option "L" ["hidraw"]
      (Opt.OptArg (\f -> opening (LinuxRawDev <$> maybe openRawHID openRawDev f)) "DEVICE")
      "open a linux hidraw device"
#endif
#ifdef USE_USB
  , Opt.Option "U" ["usb"]
      (Opt.NoArg $ opening (USBDev <$> openUSB))
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
        withDev (dev s) getColor2 (fromMaybe 0 (fmap (toEnum . read) l <|> led s)) >>= print
        return s) "LED")
      "get the current color of the LED"
  , Opt.Option "s" ["set"]
      (Opt.ReqArg (\c s -> do
        withDev (dev s) setColor2 (led s) (read c)
        return s) "RGB")
      "set the color immediately"
  , Opt.Option "t" ["time"]
      (Opt.ReqArg (\t s -> return s{ time = read t }) "DELAY")
      "set the delay/fade time for the next actions"
  , Opt.Option "f" ["fade"]
      (Opt.ReqArg (\c s -> do
        withDev (dev s) fadeToColor2 (led s) (time s) (read c)
        return s) "RGB")
      "fade to color over DELAY"
  ]

usage :: String
usage = Opt.usageInfo "Usage: blink1 [OPTIONS]" options

main :: IO State
main = do
  args <- getArgs
  r <- case Opt.getOpt Opt.RequireOrder options args of
    (r, [], []) -> return r
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure
  foldM (flip ($)) (State NoDev Nothing 0) r >>= close
