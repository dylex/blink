module Mail
  ( startMail
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Data.Monoid ((<>))
import System.Directory (getDirectoryContents)
import qualified System.Linux.Inotify as Inotify

import Segment
import Loadavg

dir :: FilePath
dir = "mail/spool/new"

file :: FilePath -> Bool
file ('.':_) = False
file [] = False
file _ = True

check :: IO Int
check = length . filter file <$> getDirectoryContents dir where

maskNew, maskOld :: Inotify.Mask a
maskNew = Inotify.in_CREATE <> Inotify.in_MOVED_TO
maskOld = Inotify.in_DELETE <> Inotify.in_MOVED_FROM

color :: Int -> Color
color 0 = RGB 0 0 0.5
color n
  | n > 0 = RGB 0 0.5 0
  | otherwise = RGB 0 0.5 0.5

mail :: Loadavg -> IO ()
mail loadavg = bracket Inotify.init Inotify.close $ \inotify -> do
  wd <- Inotify.addWatch inotify dir (maskOld <> maskNew)
  let run o n = do
        let c = color n
        when (o /= c) $ setColor loadavg c
        ev <- Inotify.getEvent inotify
        let mf m = Inotify.wd ev == wd && Inotify.hasOverlap (Inotify.mask ev) m
            g m f
              | mf m = f
              | otherwise = id
            n' = (g maskNew succ) . (g maskOld pred) $ n
        run c n'
  run 0 =<< check

startMail :: Loadavg -> IO ()
startMail l = void $ forkIO (mail l)
