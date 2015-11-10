module Craft.Apt where

import Craft
import Craft.File (File)
import qualified Craft.File as File

import Control.Monad
import Data.Maybe
import Data.List (union, (\\))
import Data.String.Utils (replace)


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
  let latests = filter ((Latest ==) . pkgVersion) pkgs
  rest <- filterM (\x -> isNothing <$> getAptPackage (pkgName x)) (pkgs \\ latests)
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
  r <- stdout . errorOnFail <$> exec cmd args
  when (null r) $ error $ "'" ++ unwords (cmd:args) ++ "'"
                          ++ " returned an empty result!"
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
      return Package { pkgName = pn
                     , pkgVersion = Version r
                     }


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
  aptGet ["remove", pkgName]


purge :: Package -> Craft ()
purge Package{..} =
  aptGet ["remove", pkgName, "--purge"]


data Deb = Deb { debFile :: File
               , debPkg  :: Package
               }
  deriving (Eq, Show)


debName :: Deb -> String
debName = pkgName . debPkg


debVersion :: Deb -> Version
debVersion = pkgVersion . debPkg


deb :: File -> Craft Deb
deb f = do
  pkg <- packageFromDebFile f
  return $ Deb f pkg


packageFromDebFile :: File -> Craft Package
packageFromDebFile f = do
  name    <- dpkgDebPackage f
  version <- dpkgDebVersion f
  return Package { pkgName    = name
                 , pkgVersion = Version version
                 }



dpkgInstall :: File -> Craft ()
dpkgInstall f =
  exec_ "/usr/bin/dpkg" ["-i", File.path f]


dpkgDebBin :: File.Path
dpkgDebBin = "/usr/bin/dpkg-deb"


dpkgDebShow :: String -> File -> Craft String
dpkgDebShow pattern f =
  expectOutput dpkgDebBin [ "--show", "--showformat", pattern, File.path f ]


dpkgDebVersion :: File -> Craft String
dpkgDebVersion = dpkgDebShow "${Version}"


dpkgDebPackage :: File -> Craft String
dpkgDebPackage = dpkgDebShow "${Package}"


instance Craftable Deb where
  checker (Deb f pkg) =
    getAptPackage (pkgName pkg) >>= \case
      Nothing  -> return Nothing
      Just pkg' -> return . Just $ Deb f pkg'

  crafter (Deb f pkg) mdeb =
    case mdeb of
      Nothing             -> dpkgInstall f
      Just (Deb _ oldpkg) ->
        when (pkgVersion oldpkg /= pkgVersion pkg) $
          dpkgInstall f

  destroyer pkg = notImplemented "destroyer Deb"


data PPA = PPA { ppaURL :: String }
  deriving (Eq, Show)


ppaURLToFilename :: String -> String
ppaURLToFilename = replace "/" "-"

instance Craftable PPA where
  checker (PPA url) = do
    fs <- filter ((> 0) . length . File.contentAsString)
          <$> File.find "/etc/apt/sources.list.d"
              ["-name", "*" ++ ppaURLToFilename url ++ "*.list"]
    if not (null fs) then
      return . Just $ PPA url
    else
      return Nothing

  crafter _      (Just _) = return ()
  crafter (PPA url) Nothing = do
    craft_ $ package "software-properties-common"
    exec_ "add-apt-repository" ["-y", "ppa:" ++ url]
    update

  destroyer (PPA url) = do
    exec_ "add-apt-repository" ["-y", "-r", "ppa:" ++ url]
    update
