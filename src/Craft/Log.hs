module Craft.Log
( module Craft.Log
, Loc
, LogSource
, LogLevel(..)
, LogStr
, toLogStr
)
where


{-
Most of this file was copied and modified from
https://github.com/kazu-yamamoto/logger/tree/master/monad-logger

Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

import qualified Data.ByteString as BS
import qualified Control.Monad.Reader as R
import Control.Monad.Free as Free
import qualified Data.Text as Text
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (ToLogStr, LogStr, toLogStr, fromLogStr)
import Control.Monad.Logger (LogSource, LogLevel(..), defaultLogStr)
import System.IO (Handle, hFlush)

import Craft.Types

craftLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> Craft ()
craftLoggerLog loc logsource level msg = do
  let logstr = toLogStr msg
  logger <- R.asks craftLogger
  R.lift $ logF $ logger loc logsource level logstr


craftDefaultLogger :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
craftDefaultLogger handle loc logsource level logstr = do
  let bs = fromLogStr $ defaultLogStr loc logsource level logstr
  BS.hPutStr handle bs
  hFlush handle


noLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
noLogger _ _ _ _ = return ()


logF :: IO () -> Free CraftDSL ()
logF action = liftF $ Log action ()


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

logWithoutLoc :: ToLogStr msg => LogSource -> LogLevel -> msg -> Craft ()
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
