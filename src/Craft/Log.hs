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
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, Loc (..), qLocation)
import System.Log.FastLogger (ToLogStr, LogStr, toLogStr, fromLogStr)
import Control.Monad.Logger (LogSource, LogLevel(..), defaultLogStr)
import System.IO (Handle, hFlush, openFile, IOMode(..))

import Craft.Types

-- |Log an error and throw a runtime exception
craftError :: Q Exp
craftError = [|\m -> $(logError) m >> error (T.unpack m)|]


notImplemented :: Q Exp
notImplemented = [|\m -> $craftError $ "Not Implemented! " <> m |]


errorOnFail :: Q Exp
errorOnFail = [|
  \case
    ExecSucc r -> return r
    ExecFail r -> $craftError $ showFailResult r|]


craftLoggerLog :: ToLogStr msg => Loc -> LogSource -> LogLevel -> msg -> Craft ()
craftLoggerLog loc logsource level msg = do
  let logstr = toLogStr msg
  ce <- R.ask
  R.lift $ logF ce loc logsource level logstr


craftDefaultLogger :: Handle -> LogLevel -> LogFunc
craftDefaultLogger handle minlevel loc logsource loglevel logstr
  | minlevel <= loglevel  = do
      let bs = fromLogStr $ defaultLogStr loc logsource loglevel logstr
      BS.hPutStr handle bs
      hFlush handle
  | otherwise = return ()


craftFileLogger :: FilePath -> LogLevel -> IO LogFunc
craftFileLogger fp minlevel = do
  h <- openFile fp WriteMode
  return $ craftDefaultLogger h minlevel


noLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
noLogger _ _ _ _ = return ()


logF :: CraftEnv -> Loc -> LogSource -> LogLevel -> LogStr -> Free CraftDSL ()
logF ce loc logsource level logstr = liftF $ Log ce loc logsource level logstr ()


logTH :: LogLevel -> Q Exp
logTH level =
    [|craftLoggerLog $(qLocation >>= liftLoc) "" $(lift level) . (id :: Text -> Text)|]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
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

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: Text -> Q Exp
logOther = logTH . LevelOther

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

-- | Generates a function that takes a 'LogSource' and 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $logDebugS "SomeSource" "This is a debug log message"
logDebugS :: Q Exp
logDebugS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelDebug (b :: Text)|]

-- | See 'logDebugS'
logInfoS :: Q Exp
logInfoS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelInfo (b :: Text)|]
-- | See 'logDebugS'
logWarnS :: Q Exp
logWarnS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelWarn (b :: Text)|]
-- | See 'logDebugS'
logErrorS :: Q Exp
logErrorS = [|\a b -> craftLoggerLog $(qLocation >>= liftLoc) a LevelError (b :: Text)|]

-- | Generates a function that takes a 'LogSource', a level name and a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $logOtherS "SomeSource" "My new level" "This is a log message"
logOtherS :: Q Exp
logOtherS = [|\src level msg -> craftLoggerLog $(qLocation >>= liftLoc) src (LevelOther level) (msg :: Text)|]

defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)

logWithoutLoc :: ToLogStr msg => LogSource -> LogLevel -> msg -> Craft ()
logWithoutLoc = craftLoggerLog defaultLoc

logDebugN :: Text -> Craft ()
logDebugN = logWithoutLoc "" LevelDebug

logInfoN :: Text -> Craft ()
logInfoN = logWithoutLoc "" LevelInfo

logWarnN :: Text -> Craft ()
logWarnN = logWithoutLoc "" LevelWarn

logErrorN :: Text -> Craft ()
logErrorN = logWithoutLoc "" LevelError

logOtherN :: LogLevel -> Text -> Craft ()
logOtherN = logWithoutLoc ""

logDebugNS :: Text -> Text -> Craft ()
logDebugNS src = logWithoutLoc src LevelDebug

logInfoNS :: Text -> Text -> Craft ()
logInfoNS src = logWithoutLoc src LevelInfo

logWarnNS :: Text -> Text -> Craft ()
logWarnNS src = logWithoutLoc src LevelWarn

logErrorNS :: Text -> Text -> Craft ()
logErrorNS src = logWithoutLoc src LevelError

logOtherNS :: Text -> LogLevel -> Text -> Craft ()
logOtherNS = logWithoutLoc
