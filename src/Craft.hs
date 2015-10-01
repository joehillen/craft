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
  { craftSourcePaths    = ["."]
  , craftPackageManager = NoPackageManager
  , craftExecEnv        = [("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin")]
  , craftExecCWD        = "/"
  }

readSourceFile :: FilePath -> IO ByteString
readSourceFile name = do
  --fps <- asks craftSourcePaths
  let fps = [] :: [FilePath]
  files <- filterM (\fp -> doesFileExist $ fp </> name) fps
  if null files then
    error $ "Source file `" ++ name ++ "` not found in file sources: "
            ++ show fps
  else
    BS.readFile $ head files </> name
