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

craftEnv :: CraftEnv NoPackageManager
craftEnv =
  CraftEnv
  { craftSourcePaths    = []
  , craftPackageManager = NoPackageManager
  --, craftExecPath       = ["/usr/sbin", "/usr/bin", "/sbin", "/bin"]
  , craftExecEnv        = []
  , craftExecCWD        = "/"
  }

readSourceFile :: FilePath -> IO ByteString
readSourceFile name = do
  --fps <- asks craftSourcePaths
  let fps = [] :: [FilePath]
  fs <- filterM (\fp -> liftIO $ doesFileExist $ fp </> name) fps
  if null fs then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    liftIO $ BS.readFile $ head fs </> name
