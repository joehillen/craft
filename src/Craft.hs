module Craft
( module Craft
, module X
, module System.FilePath
, asks
, ExitCode(..)
)
where

import           Control.Monad.Reader
import           System.FilePath
import           System.Directory
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           System.Exit (ExitCode(..))

import           Craft.Types as X
import           Craft.Actions as X
import           Craft.Watch as X
import           Craft.Exec as X
import           Craft.Helpers as X

craftEnv :: CraftEnv LocalExecuter NoPackageManager
craftEnv =
  CraftEnv
  { craftSourcePaths    = []
  , craftExecuter       = LocalExecuter
  , craftPackageManager = NoPackageManager
  , craftExecPath       = ["/usr/sbin", "/usr/bin", "/sbin", "/bin"]
  , craftExecCWD        = "/"
  }

runCraft :: (Executer ex, PackageManager pm)
         => CraftEnv ex pm -> Craft a -> IO a
runCraft env ma = runReaderT ma env

readSourceFile :: FilePath -> Craft ByteString
readSourceFile name = do
  fps <- asks craftSourcePaths
  fs <- filterM (\fp -> liftIO $ doesFileExist $ fp </> name) fps
  if null fs then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    liftIO $ BS.readFile $ head fs </> name
