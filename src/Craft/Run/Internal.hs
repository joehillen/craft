module Craft.Run.Internal where

import           Conduit                 as C
import qualified Control.Monad.Trans     as Trans
import qualified Data.Conduit.List       as CL
import           Data.Conduit.Process    (sourceProcessWithStreams)
import  Control.Monad.Reader
import           Data.Conduit.Text       as CT
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Log
import           System.Exit
import           System.Process
import qualified System.Process.ListLike as SPLL

import           Craft.Types


isSuccessCode :: ExitCode -> Bool
isSuccessCode ExitSuccess     = True
isSuccessCode (ExitFailure _) = False


runCreateProcess :: CreateProcess -> LogT IO ExecResult
runCreateProcess p = do
  (exit', stdoutRaw, stderrRaw) <- Trans.lift $ SPLL.readCreateProcessWithExitCode p "" {- stdin -}
  let stdout' = trimNL stdoutRaw
  let stderr' = trimNL stderrRaw
  return $ case exit' of
    ExitSuccess      -> Success $ SuccResult stdout' stderr' p
    ExitFailure code -> Failure $ FailResult code stdout' stderr' p


runCreateProcess_ :: String -> CreateProcess -> LogT IO ()
runCreateProcess_ src p = do
  let p' =
        p
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  let src' = "exec_|" <> T.pack src
      srcOut = src' <> "|stdout"
      srcErr = src' <> "|stderr"
  (ec, _, _) <-
    Trans.lift $
      sourceProcessWithStreams
        p'
        (CL.sourceNull)
        (pipeConsumer logger srcOut)
        (pipeConsumer logger srcErr)
  case ec of
    ExitFailure n -> $craftError $ "exec_ `"++src++"` failed with code: "++show n
    ExitSuccess   -> return ()
 where
   pipeConsumer logger s =
     decodeUtf8C =$= CT.lines =$ awaitForever (\txt ->
       (`runLogT` logger) (logDebugNS s txt))


-- | Remove a single trailing newline character from the end of the String
trimNL :: String -> String
trimNL = reverse . rmNL . reverse
 where
  rmNL []        = []
  rmNL ('\n':xs) = rmCR xs
  rmNL xs        = xs
  rmCR []        = []
  rmCR ('\r':xs) = xs
  rmCR xs        = xs
