module Mail
  ( startMail
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Monoid ((<>), mempty)
import System.Directory (getDirectoryContents)
import qualified System.Linux.Inotify as Inotify

import Util
import Key
import State
import Server

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

mail :: Server -> IO ()
mail server = bracket Inotify.init Inotify.close $ \inotify -> do
  sk <- newKey
  wd <- Inotify.addWatch inotify dir (maskOld <> maskNew)
  let run n = do
        updateServer server sk mempty{ stateMail = n }
        ev <- Inotify.getEvent inotify
        let mf m = Inotify.wd ev == wd && Inotify.hasOverlap (Inotify.mask ev) m
            g = guardEndo . mf
            n' = (g maskNew succ) . (g maskOld pred) $ n
        run n'
  run =<< check

startMail :: Server -> IO ()
startMail s = void $ forkIO (mail s)
