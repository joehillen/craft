{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Run.Internal where

import Control.Lens
import Conduit as C
import Data.Conduit.Text as CT
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import qualified System.IO as Sys.IO
import System.Process
import qualified System.Process.ListLike as SPLL
import Data.Monoid ((<>))
import Data.Streaming.Process
import Data.Streaming.Process.Internal
import System.Exit
import System.IO
import Control.Concurrent.Async

import Craft.Types
import Craft.Log


isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess     = True
isSuccess (ExitFailure _) = False


execProc :: CraftEnv -> CreateProcess -> (ExecResult -> IO a) -> IO a
execProc ce p next = do
  let logger = ce ^. craftLogger
  logger defaultLoc "exec|process" LevelDebug $ toLogStr $ showProc p
  (exit', stdoutRaw, stderrRaw) <- SPLL.readCreateProcessWithExitCode p "" {- stdin -}
  let stdout' = trimNL stdoutRaw
  let stderr' = trimNL stderrRaw
  next $ case exit' of
           ExitSuccess      -> ExecSucc $ SuccResult stdout' stderr' p
           ExitFailure code -> ExecFail $ FailResult code stdout' stderr' p


execProc_ :: CraftEnv -> String -> CreateProcess -> IO a -> IO a
execProc_ ce src p next = do
  let p' = p { std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
      src' = T.pack src
  logger defaultLoc "exec_|process" LevelDebug $ toLogStr $ showProc p
  ec <- sourceProcessWithConsumers p'
          (pipeConsumer $ "exec_|" <> src' <> "|stdout")
          (pipeConsumer $ "exec_|" <> src' <> "|stderr")
  case ec of
    ExitFailure n -> error $ "exec_ `" ++ src ++ "` failed with code: " ++ show n
    ExitSuccess   -> next
 where
   logger = ce ^. craftLogger
   pipeConsumer :: Text -> Consumer ByteString IO ()
   pipeConsumer src' = decodeUtf8C =$= CT.lines =$ awaitForever (\txt ->
     liftIO $ logger defaultLoc src' LevelDebug $ toLogStr txt)


-- | Remove a single trailing newline character from the end of the String
trimNL :: String -> String
trimNL = reverse . rmNL . reverse
 where
  rmNL [] = []
  rmNL ('\n':xs) = xs
  rmNL xs = xs


findSourceFile :: CraftEnv -> FilePath -> IO FilePath
findSourceFile ce name = do
  let fps = ce ^. craftSourcePaths
  files <- filterM (\fp -> doesFileExist $ fp </> name) fps
  if null files then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    return $ head files </> name


readSourceFileIO :: CraftEnv -> FilePath -> IO ByteString
readSourceFileIO ce name =
  BS.readFile =<< findSourceFile ce name


{-
Taken from conduit-extra https://github.com/snoyberg/conduit/blob/master/conduit-extra/Data/Conduit/Process.hs

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
instance (r ~ (), MonadIO m, o ~ ByteString) => OutputSink (ConduitM i o m r) where
    osStdStream = (\(Just h) -> return $ sourceHandle h, Just CreatePipe)
instance (r ~ (), r' ~ (), MonadIO m, MonadIO n, o ~ ByteString) => OutputSink (ConduitM i o m r, n r') where
    osStdStream = (\(Just h) -> return (sourceHandle h, liftIO $ hClose h), Just CreatePipe)


sourceProcessWithConsumers :: CreateProcess
                           -> Consumer ByteString IO () -- stdout
                           -> Consumer ByteString IO () -- stderr
                           -> IO ExitCode
sourceProcessWithConsumers cp consumerStdout consumerStderr = do
    ( ClosedStream, (sourceStdout, closeStdout)
                  , (sourceStderr, closeStderr), cph) <- streamingProcess cp
    race_ (sourceStdout $$ consumerStdout >> closeStdout)
          (sourceStderr $$ consumerStderr >> closeStderr)
    waitForStreamingProcess cph
