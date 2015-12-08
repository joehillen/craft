{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Run.Internal where

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


execProc :: CreateProcess -> (ExecResult -> IO a) -> IO a
execProc p next = do
  (exit', stdoutRaw, stderrRaw) <- SPLL.readCreateProcessWithExitCode p "" {- stdin -}
  let stdout' = trimNL stdoutRaw
  let stderr' = trimNL stderrRaw
  next $ case exit' of
           ExitSuccess      -> ExecSucc $ SuccResult stdout' stderr' p
           ExitFailure code -> ExecFail $ FailResult code stdout' stderr' p


execProc_ :: LogFunc -> CreateProcess -> IO a -> IO a
execProc_ logger p next = do
  let p' = p { std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
      src = T.pack $ showProc p'
  ec <- sourceProcessWithConsumers p'
          (pipeConsumer $ src <> "|stdout")
          (pipeConsumer $ src <> "|stderr")
  case ec of
    ExitFailure n -> error $ "exec_ failed with code: " ++ show n
    ExitSuccess   -> next
 where
   pipeConsumer :: Text -> Consumer ByteString IO ()
   pipeConsumer src = decodeUtf8C =$= CT.lines =$ awaitForever (\txt ->
     liftIO $ logger defaultLoc src LevelDebug $ toLogStr txt)



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


-- | Remove a single trailing newline character from the end of the String
trimNL :: String -> String
trimNL = reverse . rmNL . reverse
 where
  rmNL [] = []
  rmNL ('\n':xs) = xs
  rmNL xs = xs


readSourceFileIO :: [FilePath] -> FilePath -> IO ByteString
readSourceFileIO fps name = do
  files <- filterM (\fp -> doesFileExist $ fp </> name) fps
  if null files then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    BS.readFile $ head files </> name
