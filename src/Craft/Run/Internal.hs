{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Run.Internal where

import           Conduit as C
import           Control.Lens
import           Control.Monad.Logger (askLoggerIO, logDebugNS, LoggingT, runLoggingT)
import           Control.Monad.Reader
import qualified Control.Monad.Trans as Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process (sourceProcessWithStreams)
import           Data.Conduit.Text as CT
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import qualified System.Process.ListLike as SPLL

import           Craft.Types


isSuccessCode :: ExitCode -> Bool
isSuccessCode ExitSuccess     = True
isSuccessCode (ExitFailure _) = False


execProc :: CreateProcess -> (ExecResult -> LoggingT IO a) -> LoggingT IO a
execProc p next = do
  logDebugNS "exec|process" $ T.pack $ showProc p
  (exit', stdoutRaw, stderrRaw) <- Trans.lift $ SPLL.readCreateProcessWithExitCode p "" {- stdin -}
  let stdout' = trimNL stdoutRaw
  let stderr' = trimNL stderrRaw
  next $ case exit' of
           ExitSuccess      -> ExecSucc $ SuccResult stdout' stderr' p
           ExitFailure code -> ExecFail $ FailResult code stdout' stderr' p


execProc_ :: String -> CreateProcess -> LoggingT IO a -> LoggingT IO a
execProc_ src p next = do
  let p' = p { std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  logDebugNS "exec_|process" $ T.pack $ showProc p

  logger <- askLoggerIO
  let src' = "exec_|" <> T.pack src
      srcOut = src' <> "|stdout"
      srcErr = src' <> "|stderr"

  (ec, _, _) <- Trans.lift $ sourceProcessWithStreams
                               p'
                               (CL.sourceNull)
                               (pipeConsumer logger srcOut)
                               (pipeConsumer logger srcErr)
  case ec of
    ExitFailure n -> $craftError $ "exec_ `" ++ src ++ "` failed with code: " ++ show n
    ExitSuccess   -> next
 where
   pipeConsumer logger s = decodeUtf8C =$= CT.lines =$ awaitForever (\txt ->
     (`runLoggingT` logger) (logDebugNS s txt))


-- | Remove a single trailing newline character from the end of the String
trimNL :: String -> String
trimNL = reverse . rmNL . reverse
 where
  rmNL [] = []
  rmNL ('\n':xs) = xs
  rmNL xs = xs


findSourceFileIO :: CraftEnv -> FilePath -> IO [FilePath]
findSourceFileIO ce name = do
  let fps = ce ^. craftSourcePaths
  filterM (\fp -> doesFileExist $ fp </> name) fps


readSourceFileIO :: FilePath -> IO ByteString
readSourceFileIO fp = BS.readFile fp
