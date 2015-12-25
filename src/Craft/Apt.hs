module Craft.Apt where

import Control.Lens
import Craft
import Craft.File (File)
import qualified Craft.File as File

import Control.Lens
import Control.Monad
import Data.Maybe
import Data.ByteString.Lens
import Data.List (union, (\\))
import Data.String.Utils (replace)
import Formatting


data Apt = Apt
  deriving (Eq, Show)


instance PackageManager Apt where
  pkgGetter      _ = getAptPackage
  installer      _ = aptInstall
  upgrader       _ = aptInstall
  uninstaller    _ = aptRemove
  multiInstaller _ = aptMultiInstaller


aptMultiInstaller :: [Package] -> Craft ()
aptMultiInstaller []   = return ()
aptMultiInstaller pkgs = do
  let latests = filter ((Latest ==) . view pkgVersion) pkgs
  rest <- filterM (\x -> isNothing <$> getAptPackage (x ^. pkgName)) (pkgs \\ latests)
  aptInstallMult $ latests `union` rest


aptGet :: [String] -> Craft ()
aptGet args = exec_ "/usr/bin/apt-get" $ ["-q", "-y"] ++ args


update :: Craft ()
update = aptGet ["update"]


dpkgQuery :: [String] -> Craft ExecResult
dpkgQuery = exec dpkgQueryBin


dpkgQueryBin :: String
dpkgQueryBin = "/usr/bin/dpkg-query"


dpkgQueryStatus :: String -> Craft ExecResult
dpkgQueryStatus pn = dpkgQuery ["-s", pn]


expectOutput :: String -> [String] -> Craft String
expectOutput cmd args = do
  r <- view (errorOnFail . stdout) <$> exec cmd args
  when (null r) $
    $craftError $ formatToString ("'"%string%"' returned an empty result!")
                                 (unwords $ cmd:args)
  return r


dpkgQueryShow :: String -> String -> Craft String
dpkgQueryShow pattern n =
  expectOutput dpkgQueryBin [ "--show", "--showformat", pattern, n ]


dpkgQueryVersion :: String -> Craft String
dpkgQueryVersion = dpkgQueryShow "${Version}"


dpkgQueryPackage :: String -> Craft String
dpkgQueryPackage = dpkgQueryShow "${Package}"


getAptPackage :: PackageName -> Craft (Maybe Package)
getAptPackage pn =
  dpkgQueryStatus pn >>= \case
    ExecFail _ -> return Nothing
    ExecSucc _ -> Just <$> do
      r <- dpkgQueryVersion pn
      return $ Package pn (Version r)


aptInstallArgs :: [String]
aptInstallArgs = ["-o", "DPkg::Options::=--force-confold", "install"]


aptInstall :: Package -> Craft ()
aptInstall pkg = aptGet $ aptInstallArgs ++ [pkgArg pkg]


aptInstallMult :: [Package] -> Craft ()
aptInstallMult [] = return ()
aptInstallMult pkgs = aptGet $ aptInstallArgs ++ map pkgArg pkgs


pkgArg :: Package -> String
pkgArg (Package pn AnyVersion)  = pn
pkgArg (Package pn Latest)      = pn
pkgArg (Package pn (Version v)) = pn ++ "=" ++ v


aptRemove :: Package -> Craft ()
aptRemove Package{..} =
  aptGet ["remove", _pkgName]


purge :: Package -> Craft ()
purge Package{..} =
  aptGet ["remove", _pkgName, "--purge"]


data Deb = Deb { _debFile :: File
               , _debPkg  :: Package
               }
  deriving (Eq, Show)

makeLenses ''Deb

debName :: Lens' Deb String
debName = debPkg . pkgName


debVersion :: Lens' Deb Version
debVersion = debPkg . pkgVersion


deb :: File -> Craft Deb
deb f = do
  pkg <- packageFromDebFile f
  return $ Deb f pkg


packageFromDebFile :: File -> Craft Package
packageFromDebFile f = do
  name    <- dpkgDebPackage f
  version <- dpkgDebVersion f
  return $ Package name (Version version)


dpkgInstall :: File -> Craft ()
dpkgInstall f =
  exec_ "/usr/bin/dpkg" ["-i", f ^. File.path]


dpkgDebBin :: FilePath
dpkgDebBin = "/usr/bin/dpkg-deb"


dpkgDebShow :: String -> File -> Craft String
dpkgDebShow pattern f =
  expectOutput dpkgDebBin [ "--show", "--showformat", pattern, f ^. File.path ]


dpkgDebVersion :: File -> Craft String
dpkgDebVersion = dpkgDebShow "${Version}"


dpkgDebPackage :: File -> Craft String
dpkgDebPackage = dpkgDebShow "${Package}"


instance Craftable Deb where
  watchCraft d = do
    let name = d ^. debName
        ver = d ^. debVersion
        get = getAptPackage name
        error' str =
          $craftError $ formatToString ("craft Deb `"%shown%"` failed! "%string)
                                       d str
        installAndVerify = do
          dpkgInstall $ d ^. debFile
          get >>= \case
            Nothing -> error' "Package Not Found."
            Just pkg' -> do
              let ver' = pkg' ^. pkgVersion
              when (ver' /= ver) $
                error' $ formatToString ("Wrong Version: "%shown%" Expected: "%shown)
                                        ver' ver
    get >>= \case
      Nothing  -> do
        installAndVerify
        return (Created, d)

      Just pkg' -> do
        let ver' = pkg' ^. pkgVersion
        if ver' == ver then
          return (Unchanged, d)
        else do
          installAndVerify
          return (Updated, d)


data PPA = PPA { ppaURL :: String }
  deriving (Eq, Show)


findPPAFiles :: PPA -> Craft [File]
findPPAFiles (PPA url) =
  filter ((> 0) . length . (view $ File.content . _Just . unpackedChars))
          <$> File.find "/etc/apt/sources.list.d"
              ["-name", "*" ++ replace "/" "*" url ++ "*.list"]


instance Craftable PPA where
  watchCraft ppa@(PPA url) = do
    fs <- findPPAFiles ppa
    if null fs then do
      craft_ $ package "software-properties-common"
      exec_ "add-apt-repository" ["-y", "ppa:" ++ url]
      update
      return (Created, ppa)
    else
      return (Unchanged, ppa)


instance Destroyable PPA where
  watchDestroy ppa@(PPA url) = do
    fs <- findPPAFiles ppa
    if null fs then
      return (Unchanged, Nothing)
    else do
      craft_ $ package "software-properties-common"
      exec_ "add-apt-repository" ["-y", "-r", "ppa:" ++ url]
      fs <- findPPAFiles ppa
      unless (null fs) $
        $craftError $ formatToString ("destroy PPA `"%shown%"` failed! Found: "%shown)
                                     ppa fs
      update
      return (Removed, Just ppa)
