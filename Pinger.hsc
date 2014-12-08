{-# LANGUAGE ForeignFunctionInterface #-}
module Pinger
  ( startPinger
  ) where

import Control.Concurrent (forkIO, threadWaitRead)
import Control.Exception (bracket)
import Control.Monad (void, when)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_, errnoToIOError, eNOSYS)
import Foreign.C.Types (CInt(..), CULong(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import System.Posix.IO (openFd, OpenMode(ReadOnly), defaultFileFlags, closeFd)
import System.Posix.Types (Fd(..))

#include "../pinger/pingdev.h"

import Time
import Segment
import Activity
import Blinker
import Globals

color :: Float -> Color
color pt = RGB (c pt) 0 0 where
  c t
    | isInfinite t = 1
    | t >= 2.1 = 0.5
    | t > 0.1 = (t - 0.1)/4
    | otherwise = 0

foreign import ccall unsafe "ioctl" _ioctlInterval :: Fd -> CULong -> IO CInt

pingInterval :: Fd -> IO Interval
pingInterval pd = do
  i <- throwErrnoIfMinus1 "ioctl(PINGDEV_GET_INTERVAL)" $ _ioctlInterval pd (#const PINGDEV_GET_INTERVAL)
  when (i == 0) $ ioError $ errnoToIOError "ioctl(PING_GET_INTERVAL)" eNOSYS Nothing Nothing
  return $ fromIntegral i

foreign import ccall unsafe "ioctl" _ioctlPing :: Fd -> CULong -> Ptr Float -> IO CInt

ping :: Fd -> IO Float
ping pd = alloca $ \p -> do
  throwErrnoIfMinus1_ "ioctl(PINGDEV_GET_PING)" $ _ioctlPing pd (#const PINGDEV_GET_PING) p
  peek p

pinger :: Globals -> IO ()
pinger globals = bracket (openFd "/dev/ping" ReadOnly Nothing defaultFileFlags) (closeFd) $ \pd -> do
  k <- newKey (blinker1 globals)
  i <- pingInterval pd
  let run c = do
        threadWaitRead pd
        p <- ping pd
        let c' = color p
            up :: Maybe Sequence -> Maybe Sequence
            up a = Just (Sequence [Segment (segColor $ segment a) i c', solid c'])
        when (c' /= c) $ updateAct k up
        run c'
        
  run 0

startPinger :: Globals -> IO ()
startPinger g = void $ forkIO (pinger g)
