module Blink where

import Control.Concurrent
import qualified Data.Map.Strict as Map
import System.Hardware.Blink1
import System.Hardware.Blink1.Linux

roundRGB :: RGB Float -> RGB8
roundRGB = fmap round

floatRGB :: RGB8 -> RGB Float
floatRGB = fmap fromIntegral

data Segment = Segment
  { segStart, segEnd :: !RGB8
  , segLen :: !Delay
  }

interp :: Segment -> Delay -> RGB8
interp (Segment s e l) r
  | r <= 0 = e
  | r >= l = s
  | otherwise = e + roundRGB (fmap (f *) (floatRGB s - floatRGB e))
  where f = realToFrac r / realToFrac l :: Float

type SoftLED = String

data LED
  = Hard !Int
  | Soft SoftLED
  deriving (Eq, Ord)

newtype Owner = OwnerThread ThreadID deriving (Eq, Ord)

data Color
  = ColorRGB !RGB8
  | ColorSoft !SoftLED

data Activity 
  = ActivityFixed 
    { actColor :: !Color
  | ActivityFade
    { actColor :: !Color
    , actDelay :: !Delay
    , actColor' :: !Color
    }

newtype State = State
  { stateActs :: Map.Map Owner Activity
  , stateFades :: [(Delay, Owner)]
  , stateFixed :: [Owner]
  }

stateUpdate :: Owner -> Maybe Activity -> State -> State
stateUpdate own new state = state{ stateActs = acts } where
  (old, acts) = updateLookupWithKey (\_ _ -> act) own (stateActs state)
  remove ActivityFixed{} = state{ stateFixed = 

type States :: Map.Map LED State

data Blink = Blink
  { statesMVar :: MVar States
  , controlThread :: ThreadID
  }

type BlinkIO = ReaderT State IO

changeStates :: (States -> States) -> BlinkIO ()
changeStates f = do

setAct :: LED -> Owner -> Maybe Activity -> BlinkIO ()
setAct led own act = do
  blink <- ask
  motifyMVar_ (statesMVar blink) $ liftM $ Map.adjust (maybe (Map.delete own) (Map.insert own) act) led
  -- throwTo (controlThread blink) ...

class Monad m => Time m where
  now :: m Time

data Eval = Eval
  { evalTime :: Time
  , evalStates :: States
  }

type EvalM a = Eval -> a

instance Time EvalM where
  now = reader evalTime

evalColor :: Color -> EvalM RGB8
evalColor (ColorRGB c) = return c
evalColor (ColorSoft s) = evalLED (Soft s)

evalAct :: Activity -> EvalM RGB8
evalAct Activity{ actStart = s, actColor = c, actFade = f } = do
  t <- now
  if s > t
    then return 0
    else maybe (evalColor c) (\(d, e) -> interp ) f

evalLED :: LED -> EvalM RGB8
evalLED led = foldM (\c a -> c + evalAct t a) 0 . elems . (Map.! led) =<< reader evalStates

eval :: LED -> BlinkIO RGB8
eval led = do
  evalLED led . Eval t <$> readMVar =<< reader statesMVar
