
import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (liftM3, unless)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS (length)
import Data.List (foldl')
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString.Lazy as Net.BS
import qualified System.Console.GetOpt as Opt
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Error (mkIOError, fullErrorType)
import Text.Read (readMaybe)

import System.Hardware.Blink1.Types (RGB8, black, Delay)

import Segment (Segment1(..))
import Command (Command(..))

data Options = Options
  { optSocket :: FilePath
  , optCommand :: Command
  }

defaultOptions :: Options
defaultOptions = Options
  { optSocket = "/tmp/.blink.ctl"
  , optCommand = CmdSequence 0 1 []
  }

optCommand' :: (Command -> Command) -> (Options -> Options)
optCommand' f o = o{ optCommand = f (optCommand o) }

options :: [Opt.OptDescr (Options -> Options)]
options =
  [ Opt.Option "S" ["socket"]
      (Opt.ReqArg (\s o -> o{ optSocket = s }) "PATH")
      ("conenct to socket PATH" ++ def (Just . optSocket))
  , Opt.Option "l" ["led"]
      (Opt.ReqArg (\l -> optCommand' (\c -> c{ cmdWhich = toEnum (read l) })) "INDEX")
      ("run the sequence on LED INDEX" ++ def (Just . show . fromEnum . cmdWhich . optCommand))
  , Opt.Option "r" ["repeat"]
      (Opt.OptArg (\r -> optCommand' (\c -> c{ cmdRepeat = maybe (-1) read r })) "COUNT")
      ("repeat the sequence COUNT times (or forever)" ++ def (Just . show . cmdRepeat . optCommand))
  ] where
  def f = maybe "" ((" [" ++) . (++ "]")) (f defaultOptions)

usage :: String
usage = Opt.usageInfo "Usage: blinkh [OPTIONS] SEQUENCE\n\
  \Run (replace) a command sequence in a blinkhd.\n\n\
  \  SEQUENCE            list of (comma-delimited) segments:\n\
  \    COLOR             solid COLOR forever\n\
  \    COLOR LEN         solid COLOR for LEN\n\
  \    COLOR LEN COLOR   fade from COLOR to COLOR in LEN\n\
  \    LEN COLOR         fade from previous color to COLOR in LEN\n\
  \  COLOR               [#]RGB or [#]RRGGBB hex triplet\n\
  \  LEN                 SECONDS[s], 100cs, or 1000ms\n" options

type Parse = Either String
type Parser a = String -> Parse a

infixr 2 <|>
(<|>) :: Parse a -> Parse a -> Parse a
Left e1 <|> Left e2 = Left (e1 ++ "\n" ++ e2)
Left _ <|> r = r
l <|> _ = l

parser :: Read a => String -> Parser a
parser t s = maybe (Left ("invalid " ++ t ++ ": " ++ s)) Right $ readMaybe s

parseArgs :: [String] -> Either String (Options -> Options)
parseArgs args = set <$> run black (map words $ split $ unwords args) where
  set r = optCommand' (\c -> c{ cmdSequence = cmdSequence c ++ r })

  run :: RGB8 -> [[String]] -> Parse [Segment1]
  run _ [] = return []
  run p [[]] = return [Segment1Solid p]
  run p [[a]] = return . either Segment1Solid (solid p) <$> colorOrLen a
  run p (s:l) = do
    f <- seg p s
    (f :) <$> run (seg1End f) l
  seg :: RGB8 -> [String] -> Parse Segment1
  seg p [l] = solid p <$> len l
  seg p [a1,a2] = either 
    (\c -> solid c <$> len a2)
    (\l -> Segment1Fade p l <$> color a2)
    =<< colorOrLen a1
  seg _ [s,l,e] = liftM3 Segment1Fade (color s) (len l) (color e)
  seg _ s = fail $ "invalid segment: " ++ unwords s
  solid p l = Segment1Fade p l p
  color :: Parser RGB8
  color s@('#':_) = parser "color" s
  color s = color ('#':s)
  len :: Parser Delay
  len = parser "delay"
  colorOrLen :: Parser (Either RGB8 Delay)
  colorOrLen a = Left <$> color a <|> Right <$> len a

  split :: String -> [String]
  split [] = []
  split (',':l) = [] : split l
  split (x:s) = pre (split s) where
    pre [] = [[x]]
    pre (h:r) = ((x:h):r)

runArgs :: (o, [String], [String]) -> (o, Options -> Options, [String])
runArgs (o, a, es) = either (\e -> (o, id, es ++ [e])) (\p -> (o, p, es)) $ parseArgs a

main :: IO ()
main = do
  args <- getArgs
  opts <- case runArgs $ Opt.getOpt Opt.RequireOrder options args of
    (o, a, []) -> return $ a $ foldl' (flip ($)) defaultOptions o
    (_, _, err) -> do
      mapM_ putStrLn err
      putStrLn usage
      exitFailure

  bracket (Net.socket Net.AF_UNIX Net.Datagram Net.defaultProtocol) Net.close $ \sock -> do
  Net.connect sock (Net.SockAddrUnix (optSocket opts))

  let send cmd = do
        r <- Net.BS.send sock d
        unless (r == BS.length d) $ ioError $ mkIOError fullErrorType "send" Nothing (Just (optSocket opts))
        where d = encode cmd

  -- print (cmdSequence $ optCommand opts)
  send (optCommand opts)

  return ()
