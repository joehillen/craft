module Craft.Pip where

import Craft hiding (package, latest)
import qualified Craft
import qualified Craft.File as File
import Control.Monad (unless)

import Text.Megaparsec


newtype PipPackage = PipPackage Package
  deriving (Eq, Show)


setup :: Craft ()
setup = do
  let pippkg = Craft.package "python-pip"
  File.exists "/usr/local/bin/pip" >>= flip unless (craft_ pippkg)
  craft_ $ latest "pip"
  destroy_ pippkg


package :: PackageName -> PipPackage
package pn = PipPackage $ Package pn AnyVersion


latest :: PackageName -> PipPackage
latest pn = PipPackage $ Package pn Latest


get :: PackageName -> Craft (Maybe PipPackage)
get pn = do
  results <- withPath ["/usr/local/bin", "/usr/bin"] $
               parseExec pipShowParser stdout "pip" ["show", pn]
  if null results then
    return Nothing
  else return $
    case lookup "Version" results of
      Nothing -> error "`pip show` did not return a version"
      Just version ->
        Just . PipPackage $ Package pn $ Version version


-- TESTME
pipShowParser :: Parsec String [(String, String)]
pipShowParser = many $ kv <* many eol
 where
  kv :: Parsec String (String, String)
  kv = do
    key <- some $ noneOf ":"
    char ':' >> space
    value <- many $ noneOf "\n"
    return (key, value)


pip :: [String] -> Craft ()
pip args = withPath ["/usr/local/bin", "/usr/bin"] $ exec_ "pip" args


pkgArgs :: PipPackage -> [String]
pkgArgs (PipPackage (Package pn pv)) = go pv
 where
  go AnyVersion = [pn]
  go Latest = ["--upgrade", pn]
  go (Version v) = ["--ignore-installed", pn ++ "==" ++ v]


pipInstall :: PipPackage -> Craft ()
pipInstall pkg = pip $ "install" : pkgArgs pkg


instance Craftable PipPackage where
  checker (PipPackage name) = get $ pkgName name
  crafter pkg _ = pipInstall pkg
  destroyer = notImplemented "destroyer PipPackage"
