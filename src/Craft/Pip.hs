module Craft.Pip where

import Craft hiding (package, latest)
import qualified Craft
import qualified Craft.File as File

import Text.Megaparsec


newtype PipPackage = PipPackage Package
  deriving (Eq, Show)


setup :: Craft ()
setup = do
  mapM_ (craft_ . Craft.package) ["libffi-dev", "libssl-dev", "python-dev"]
  let pippkg = Craft.package "python-pip"
  File.exists "/usr/local/bin/pip" >>= flip unless (craft_ pippkg)
  mapM_ (craft_ . package) ["pyopenssl", "ndg-httpsclient", "pyasn1"]
  craft_ $ latest "pip"
  destroy_ pippkg


package :: PackageName -> PipPackage
package pn = PipPackage $ Package pn AnyVersion


latest :: PackageName -> PipPackage
latest pn = PipPackage $ Package pn Latest


get :: PackageName -> Craft (Maybe PipPackage)
get pn = do
  r <- withPath ["/usr/local/bin", "/usr/bin"] $ exec "pip" ["show", pn]
  case r of
    ExecFail _     -> return Nothing
    ExecSucc succr ->
      let results = parseExecResult r pipShowParser (stdout succr) in
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
  watchCraft ppkg@(PipPackage pkg) = do
    let name = pkgName pkg
        ver = pkgVersion pkg
        verify =
          get name >>= \case
            Nothing -> error $ "craft PipPackage `" ++ name ++ "` failed! Not Found."
            Just ppkg'@(PipPackage pkg') -> do
              let newver = pkgVersion pkg'
              case ver of
                Version _ ->
                  when (newver /= ver) $
                    error $ "craft PipPackage `" ++ name ++ "` failed!"
                          ++ " Wrong Version: " ++ show newver
                          ++ " Expected: " ++ show ver
                _ -> return ()
              return ppkg'
    get name >>= \case
      Nothing -> do
        pipInstall ppkg
        ppkg' <- verify
        return (Created, ppkg')
      Just (PipPackage pkg') -> do
        let ver' = pkgVersion pkg'
        if ver' == ver then
          return (Unchanged, ppkg)
        else do
          pipInstall ppkg
          ppkg' <- verify
          return (Updated, ppkg')
