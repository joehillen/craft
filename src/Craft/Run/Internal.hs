module Craft.Run.Internal where

import Control.Monad (filterM)
import Control.Monad.Reader (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory
import System.Exit
import System.FilePath
import qualified System.IO as Sys.IO
import System.Process hiding ( readCreateProcessWithExitCode
                             , readProcessWithExitCode)
import System.Process.ListLike

import Craft.Types
import Craft.Helpers


isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess     = True
isSuccess (ExitFailure _) = False


execProc_ :: CreateProcess -> IO a -> IO a
execProc_ p next = do
  msg "exec_" $ showProc p
  (_, _, _, ph) <- liftIO $ createProcess p
  liftIO (waitForProcess ph) >>= \case
    ExitFailure n -> do
      flushStdout
      error $ "exec_ failed with code: " ++ show n
    ExitSuccess   -> do
      flushStdout
      next


execProc :: CreateProcess -> (ExecResult -> IO a) -> IO a
execProc p next = do
  msg "exec" $ showProc p
  (exit', stdoutRaw, stderrRaw) <- readCreateProcessWithExitCode p "" {- stdin -}
  let stdout' = trimNL stdoutRaw
  let stderr' = trimNL stderrRaw
  next $ case exit' of
           ExitSuccess      -> ExecSucc $ SuccResult stdout' stderr' p
           ExitFailure code -> ExecFail $ FailResult code stdout' stderr' p


flushStdout :: IO ()
flushStdout = Sys.IO.hFlush Sys.IO.stdout


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
