module Craft.Log where

import Data.ByteString (ByteString)
import qualified Control.Monad.Reader as R
import Control.Monad.Free as Free
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (ToLogStr, toLogStr, fromLogStr)
import Control.Monad.Logger (Loc(..), LogSource, LogLevel(..), defaultLogStr)
import System.IO (Handle)

import Craft.Types

craftLoggerLog :: ToLogStr msg
               => Loc -> LogSource -> LogLevel -> msg -> Craft ()
craftLoggerLog loc logsource level m = do
  let bs = defaultLogStr loc logsource level $ toLogStr m
  h <- R.asks craftLogHandle
  R.lift $ logF h $ fromLogStr bs


logF :: Handle -> ByteString -> Free (CraftDSL pm) ()
logF h bs = liftF $ Log h bs ()


logTH :: LogLevel -> Q Exp
logTH level =
    [|craftLoggerLog $(qLocation >>= liftLoc) "" $(lift level) . (id :: String -> String)|]

-- | Generates a function that takes a 'String' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo
-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn
-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'String' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: String -> Q Exp
logOther = logTH . LevelOther . Text.pack

-- | Lift a location into an Exp.
--
-- Since 0.3.1
liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]

-- | Generates a function that takes a 'LogSource' and 'String' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
logDebugS :: Q Exp
logDebugS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelDebug (b :: String)|]

-- | See 'logDebugS'
logInfoS :: Q Exp
logInfoS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelInfo (b :: String)|]
-- | See 'logDebugS'
logWarnS :: Q Exp
logWarnS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelWarn (b :: String)|]
-- | See 'logDebugS'
logErrorS :: Q Exp
logErrorS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelError (b :: String)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'String' and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level msg -> craftLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) (msg :: String)|]

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

logWithoutLoc :: (ToLogStr msg) => LogSource -> LogLevel -> msg -> Craft ()
logWithoutLoc = craftLoggerLog defaultLoc

logDebugN :: String -> Craft ()
logDebugN = logWithoutLoc "" LevelDebug

logInfoN :: String -> Craft ()
logInfoN = logWithoutLoc "" LevelInfo

logWarnN :: String -> Craft ()
logWarnN = logWithoutLoc "" LevelWarn

logErrorN :: String -> Craft ()
logErrorN = logWithoutLoc "" LevelError

logOtherN :: LogLevel -> String -> Craft ()
logOtherN = logWithoutLoc ""

logDebugNS :: String -> String -> Craft ()
logDebugNS src = logWithoutLoc (Text.pack src) LevelDebug

logInfoNS :: String -> String -> Craft ()
logInfoNS src = logWithoutLoc (Text.pack src) LevelInfo

logWarnNS :: String -> String -> Craft ()
logWarnNS src = logWithoutLoc (Text.pack src) LevelWarn

logErrorNS :: String -> String -> Craft ()
logErrorNS src = logWithoutLoc (Text.pack src) LevelError

logOtherNS :: String -> LogLevel -> String -> Craft ()
logOtherNS = logWithoutLoc . Text.pack
