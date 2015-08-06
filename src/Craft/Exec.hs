module Craft.Exec where

import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.List (intercalate)
import           System.Exit
import           System.Process hiding ( readCreateProcessWithExitCode
                                       , readProcessWithExitCode)
import           System.Process.ListLike
import           Text.Parsec
import           Text.Parsec.String (Parser)

import           Craft.Types
import           Craft.Helpers


type Command = FilePath
type Args = [String]
type StdErr = String
type StdOut = String

data LocalExecuter = LocalExecuter

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess     = True
isSuccess (ExitFailure _) = False

localExec :: Command -> Args -> Craft (ExitCode, StdOut, StdErr)
localExec prog args = do
  p <- craftProc prog args
  liftIO $ readCreateProcessWithExitCode p "" {- stdin -}

localExec_ :: Command -> Args -> Craft ()
localExec_ prog args= do
  (_, _, _, ph) <- liftIO . createProcess =<< craftProc prog args
  liftIO (waitForProcess ph) >>= \case
    ExitSuccess -> return ()
    ExitFailure n -> error $ "exec_ failed with code: " ++ show n

instance Executer LocalExecuter where
  executer   _      = localExec
  executer_  _      = localExec_
  fileReader _ fp   = liftIO $ BS.readFile fp
  fileWriter _ fp c = liftIO $ BS.writeFile fp c

exec_ :: Command -> Args -> Craft ()
exec_ prog args = do
  ex <- asks craftExecuter
  executer_ ex prog args

exec :: FilePath -> [String] -> Craft (ExitCode, String, String)
exec prog args = do
  ex <- asks craftExecuter
  executer ex prog args


-- | better than grep
parseExec :: Parser a -> Command -> Args -> Craft a
parseExec parser prog args = do
  (exit, stdout, stderr) <- exec prog args
  let exitStr = case exit of
                  ExitSuccess -> "0"
                  ExitFailure c -> show c
  case parse parser (unwords $ prog:args) stdout of
    Left err -> error $
      "parseExec failed!\n"
      ++ show err ++ "\n"
      ++ "stdout:\n"   ++ stdout ++ "\n" ++
      if not (null stderr) then "stderr:\n"   ++ stderr ++ "\n" else ""
      ++ "exit code: " ++ exitStr
    Right r -> return r

craftProc :: Command -> Args -> Craft CreateProcess
craftProc prog args = do
  path <- intercalate ":" <$> asks craftExecPath
  cwd'  <- asks craftExecCWD
  liftIO $ msg "exec" $
    unwords ( ("PWD=" ++ cwd')
            : ("PATH=" ++ path)
            : prog
            : args
            )
  return $ (proc prog args) { cwd           = Just cwd'
                            , env           = Just [("PATH", path)]
                            , close_fds     = True
                            , create_group  = True
                            , delegate_ctlc = False
                            }

withPath :: [FilePath] -> Craft a -> Craft a
withPath paths = local (\r -> r { craftExecPath = paths })

withCWD :: FilePath -> Craft a -> Craft a
withCWD path = local (\r -> r { craftExecCWD = path })
