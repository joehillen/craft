module Craft.Pip where

import           Formatting      hiding (char)
import qualified Formatting      as F
import           Text.Megaparsec

import           Craft           hiding (latest, package)
import qualified Craft
import qualified Craft.File      as File


newtype PipPackage = PipPackage Package
  deriving (Eq, Show)

pipfp :: Path Abs FileP
pipfp = $(mkAbsFile "/usr/local/bin/pip")

setup :: Craft ()
setup = do
  craft_ $ map Craft.package ["libffi-dev", "libssl-dev", "python-dev"]
  let pippkg = Craft.package "python-pip"
  File.exists pipfp >>= flip unless (craft_ pippkg)
  craft_ $ map package ["pyopenssl", "ndg-httpsclient", "pyasn1"]
  craft_ $ latest "pip"
  destroy_ pippkg
  craft_ $ latest "setuptools"
  destroy_ $ Craft.package "python-requests"


package :: PackageName -> PipPackage
package pn = PipPackage $ Package pn AnyVersion


latest :: PackageName -> PipPackage
latest pn = PipPackage $ Package pn Latest


get :: PackageName -> Craft (Maybe PipPackage)
get pn = do
  r <- withPath [$(mkAbsDir "/usr/local/bin"), $(mkAbsDir "/usr/bin")] $ exec "pip" ["show", pn]
  case r of
    ExecFail _     -> return Nothing
    ExecSucc succr -> do
      results <- parseExecResult r pipShowParser (succr ^. stdout)
      if null results then
        return Nothing
      else case lookup "Version" results of
        Nothing      -> $craftError "`pip show` did not return a version"
        Just version -> return . Just
                               . PipPackage
                               $ Package pn $ Version version


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
pip args = withPath [$(mkAbsDir "/usr/local/bin"), $(mkAbsDir "/usr/bin")] $ exec_ "pip" args


pkgArgs :: PipPackage -> [String]
pkgArgs (PipPackage (Package pn pv)) = go pv
 where
  go AnyVersion  = [pn]
  go Latest      = ["--upgrade", pn]
  go (Version v) = ["--ignore-installed", pn ++ "==" ++ v]


pipInstall :: PipPackage -> Craft ()
pipInstall pkg = pip $ "install" : pkgArgs pkg


instance Craftable PipPackage PipPackage where
  watchCraft ppkg@(PipPackage pkg) = do
    let name = pkg ^. pkgName
        ver = pkg ^. pkgVersion
        verify =
          get name >>= \case
            Nothing -> $craftError $
                         "craft PipPackage `"++name++"` failed! Not Found."
            Just ppkg'@(PipPackage pkg') -> do
              let newver = pkg' ^. pkgVersion
              case ver of
                Version _ ->
                  when (newver /= ver) $
                    $craftError
                      $ formatToString
                        ("craft PipPackage `"%F.string%"` failed! Wrong Version: "%shown%" Expected: "%shown)
                        name newver ver
                _ -> return ()
              return ppkg'

    get name >>= \case
      Nothing -> do
        pipInstall ppkg
        ppkg' <- verify
        return (Created, ppkg')
      Just (PipPackage pkg') -> do
        let ver' = pkg' ^. pkgVersion
        if ver' == ver then
          return (Unchanged, ppkg)
        else do
          pipInstall ppkg
          ppkg' <- verify
          return (Updated, ppkg')
