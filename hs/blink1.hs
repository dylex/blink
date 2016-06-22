{-# LANGUAGE CPP, RankNTypes, ExistentialQuantification, TupleSections #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$))
#endif
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Monad (guard, foldM)
import Data.Maybe (fromMaybe, isNothing)
import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
  { dev :: Maybe b
  , led :: Maybe LED
  , patternStep :: Maybe PatternStep
  }

noDev :: Maybe Blink1Dummy
noDev = Nothing

withDev :: State -> (forall b . Blink1 b => b -> a) -> a
withDev State{ dev = Nothing } _ = error "No device opened"
withDev State{ dev = Just d } f = f d

opening :: Blink1 b => IO b -> State -> IO State
opening o (State Nothing l n) = o >>= \b -> return (State (Just b) l n)
opening o s = close s >>= opening o

close :: State -> IO State
close (State (Just b) l n) = State noDev l n <$ closeBlink1 b
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
  , Opt.Option "p" ["pattern-step"]
      (Opt.ReqArg (\n s -> return s{ patternStep = Just $ toEnum $ read n }) "STEP")
      "operate on pattern STEP for next actions"
  , Opt.Option "l" ["led"]
      (Opt.OptArg (\l s -> return s{ led = fmap (toEnum . read) l }) "INDEX")
      "apply the next actions only to LED INDEX [all]"
  , Opt.Option "g" ["get"]
      (Opt.OptArg (\l s -> do
        maybe
          (withDev s getColor2 (fromMaybe 0 (fmap (toEnum . read) l <|> led s)) >>= print)
          (\n -> withDev s getPattern n >>= print)
          (guard (isNothing l) >> patternStep s)
        return s) "LED")
      "get the current (or pattern STEP) color of the LED"
  , Opt.Option "s" ["set"]
      (Opt.ReqArg (\c s -> do
        let f = read c
        maybe
          (if fadeDelay f == 0 && isNothing (led s)
            then withDev s setColor2 (led s) (fadeRGB f)
            else withDev s fadeToColor2 (led s) f)
          (\n -> withDev s setPattern n f)
          (patternStep s)
        return s) "RGB[@DELAY]")
      "set LED (or pattern STEP) to fade to color RGB over DELAY [immediately]"
  , Opt.Option "W" ["write-patterns"]
      (Opt.NoArg $ run savePatterns2 return)
      "commit changed patterns to flash (mk2 only)"
  , Opt.Option "r" ["play-pattern"]
      (Opt.OptArg (\a s -> do
        let (m, r) = maybe (0, 0) ((read *** rep) . break ('*'==)) a
            rep ('*':x) = read x
            rep _ = 0
        withDev s playPattern2 (maybe (m, 0) (, m) $ patternStep s) r
        return s) "STEP[*REP]")
      "start playing pattern from STEP (after -p: to STEP) [0] REP times [inf]"
  , Opt.Option "e" ["stop-pattern"]
      (Opt.NoArg $ \s -> withDev s playPattern Nothing >> return s)
      "stop any playing pattern"
  , Opt.Option "q" ["query-pattern"]
      (Opt.NoArg $ \s -> withDev s getPlaying2 >>= print >> return s)
      "report current play state"
  , Opt.Option "d" ["server-down"]
      (Opt.OptArg (\a s -> do
        let (m, t) = maybe (0, 1) ((read *** del) . break ('@'==)) a
            del ('@':x) = read x
            del _ = 1
        withDev s setServerDown2 $ Just (t, True, Just (maybe (m, 0) (, m) $ patternStep s))
        return s) "STEP[@DELAY]")
      "enable serverdown playing from STEP (after -p: to STEP) [0]"
  , Opt.Option "u" ["server-up"]
      (Opt.NoArg $ \s -> withDev s setServerDown Nothing >> return s)
      "disable serverdown playing"
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
  foldM (flip ($)) (State noDev Nothing Nothing) r >>= close
