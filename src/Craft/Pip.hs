module Craft.Pip where

import           Control.Lens         hiding (noneOf)
import           Data.Void            (Void)
import           Formatting           hiding (char)
import qualified Formatting           as F
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Craft                hiding (latest, package)
import qualified Craft
import qualified Craft.Directory      as Dir
import qualified Craft.File           as File


newtype PipPackage = PipPackage Package
  deriving (Eq, Show)


usrLocalBin :: AbsDirPath
usrLocalBin = $(mkAbsDir "/usr/local/bin/")

pipfp :: AbsFilePath
pipfp = $(mkAbsFile "/usr/local/bin/pip")

setup :: Craft ()
setup = do
  craft_ $ map Craft.package ["libffi-dev", "libssl-dev", "python-dev"]
  let pippkg = Craft.package "python-pip"
  unlessM (File.exists pipfp) $ craft_ pippkg
  craft_ $ map package ["pyopenssl", "ndg-httpsclient", "pyasn1"]
  craft_ $ latest "pip"
  destroy_ pippkg
  withPath [usrLocalBin] $
    mapM_ (craft_ . latest)
      [ "setuptools"
      , "virtualenv"
      ]
  destroy_ $ Craft.package "python-requests"


data VirtualEnv
  = VirtualEnv
  { _vePath     :: AbsDirPath
  , _vePackages :: [PipPackage]
  }


virtualenv :: AbsDirPath -> VirtualEnv
virtualenv vepath =
  VirtualEnv
  { _vePath     = vepath
  , _vePackages = []
  }


package :: PackageName -> PipPackage
package pn = PipPackage $ Package pn AnyVersion


latest :: PackageName -> PipPackage
latest pn = PipPackage $ Package pn Latest


get :: PackageName -> Craft (Maybe PipPackage)
get pn = do
  r <- exec "pip" ["show", pn]
  case r of
    Failure _     -> return Nothing
    Success succr -> do
      results <- parseExecResult r pipShowParser (succr ^. stdout)
      if null results
        then return Nothing
        else
          case lookup "Version" results of
            Nothing      ->
              $craftError "`pip show` did not return a version"
            Just version ->
              return . Just . PipPackage $ Package pn $ Version version


-- TESTME
pipShowParser :: Parsec Void String [(String, String)]
pipShowParser = many $ kv <* many eol
 where
  kv :: Parsec Void String (String, String)
  kv = do
    key <- some $ noneOf [':']
    char ':' >> space
    value <- many $ noneOf ['\n']
    return (key, value)


pip :: [String] -> Craft ()
pip = exec_ "pip"


pkgArgs :: PipPackage -> [String]
pkgArgs (PipPackage (Package pn pv)) = go pv
 where
  go AnyVersion  = [pn]
  go Latest      = ["--upgrade", pn]
  go (Version v) = ["--ignore-installed", pn ++ "==" ++ v]


pipInstall :: PipPackage -> Craft ()
pipInstall pkg = pip $ "install" : pkgArgs pkg

makeLenses ''VirtualEnv

instance Craftable PipPackage PipPackage where
  watchCraft ppkg@(PipPackage pkg) = do
    let name = pkg ^. pkgName
    let ver = pkg ^. pkgVersion
    let verify =
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
        if ver' == ver
        then return (Unchanged, ppkg)
        else do
          pipInstall ppkg
          ppkg' <- verify
          return (Updated, ppkg')


instance Craftable VirtualEnv VirtualEnv  where
  watchCraft ve = do
    let vepath = ve^.vePath
    vePathExists <- Dir.exists vepath
    unless vePathExists $ do
      exec_ "virtualenv" [fromAbsDir vepath]
    r <-
      withPath [vepath] $
        mapM watchCraft $ ve^.vePackages
    let (w, pkgs) = (unzip r) & _1 %~ mconcat
    let ve' = ve { _vePackages = pkgs }
    return $
      if vePathExists
      then (w, ve')
      else (Created, ve')
