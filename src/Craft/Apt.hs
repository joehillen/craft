module Craft.Apt where

import           Craft
import Craft.File (File)
import qualified Craft.File as File

import           Control.Monad
import           Data.Maybe
import           Data.List (union, (\\))

data Apt = Apt
  deriving (Eq, Show)

instance PackageManager Apt where
  pkgGetter      _ = getAptPackage
  installer      _ = aptInstall
  upgrader       _ = aptInstall
  uninstaller    _ = aptRemove
  multiInstaller _ = aptMultiInstaller

aptMultiInstaller :: [Package] -> Craft ()
aptMultiInstaller []    = return ()
aptMultiInstaller pkgs  = do
  let latests = filter ((Latest ==) . pkgVersion) pkgs
  rest <- filterM (\x -> isNothing <$> getAptPackage (pkgName x)) (pkgs \\ latests)
  aptInstallMult $ latests `union` rest

aptGet :: [String] -> Craft ()
aptGet args = exec_ "/usr/bin/apt-get" $ ["-q", "-y"] ++ args

update :: Craft ()
update = aptGet ["update"]

dpkgQuery :: [String] -> Craft ExecResult
dpkgQuery = exec "/usr/bin/dpkg-query"


getAptPackage :: PackageName -> Craft (Maybe Package)
getAptPackage pn = do
  r <- dpkgQuery ["--show", "--showformat", "'${Version}'", pn]
  return $ case exitcode r of
    ExitFailure _ -> Nothing
    ExitSuccess -> Just $ Package { pkgName = pn
                                  , pkgVersion = Version $ stdout r
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

data Deb = Deb File
  deriving (Eq, Show)

dpkgInstall :: File -> Craft ()
dpkgInstall f =
  exec_ "/usr/bin/dpkg" ["-i", File.path f]

dpkgDebBin :: File.Path
dpkgDebBin = "/usr/bin/dpkg-deb"

packageFromDeb :: Deb -> Craft Package
packageFromDeb (Deb f) = do
  let dpkgExec pattern = stdout <$> exec dpkgDebBin
                                         [ "--show", "--showformat"
                                         , pattern, File.path f
                                         ]
  name    <- dpkgExec "${Package}"
  when (null name) $ error "packageFromDeb 'name' is empty"
  version <- dpkgExec "${Version}"
  when (null version) $ error "packageFromDeb 'version' is empty"
  return Package { pkgName    = name
                 , pkgVersion = Version version
                 }

instance Craftable Deb where
  checker deb = do
    pkg <- packageFromDeb deb
    checker pkg >>= \case
      Nothing -> return Nothing
      Just  _ -> return $ Just deb
  crafter (Deb f) = dpkgInstall f
  remover deb = notImplemented "remover Deb"
