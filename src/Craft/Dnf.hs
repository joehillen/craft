module Craft.Dnf where

import Craft


import Data.Maybe
import Data.List (union, (\\))
import Control.Monad
import System.Exit

data DnfPackage = DnfPackage Package
                  deriving (Eq, Show)

package :: PackageName -> DnfPackage
package n = DnfPackage $ Craft.Package.package n

latest :: PackageName -> DnfPackage
latest n = DnfPackage $ Package n Latest

dnf :: [Text] -> Craft ()
dnf args = exec_ "/usr/bin/dnf" $ ["-q", "-y"] ++ args

update :: Craft ()
update = do
  dnf ["--refresh", "makecache"]

rpmQuery :: [Text] -> Craft (ExitCode, Text, Text)
rpmQuery args = do
  (exit, stdout, stderr) <-
    exec "/usr/bin/rpm" args
  return (exit, stdout, stderr)

dnfInstallArgs :: [Text]
dnfInstallArgs = ["install"]

dnfInstall :: DnfPackage -> Craft ()
dnfInstall pkg = dnf $ dnfInstallArgs ++ [pkgArg pkg]

dnfInstallMult :: [DnfPackage] -> Craft ()
dnfInstallMult [] = return ()
dnfInstallMult pkgs = dnf $ dnfInstallArgs ++ map pkgArg pkgs

pkgArg :: DnfPackage -> Text
pkgArg (DnfPackage (Package pn AnyVersion))  = pn
pkgArg (DnfPackage (Package pn Latest))      = pn
pkgArg (DnfPackage (Package pn (Version v))) = pn ++ "-" ++ v

instance Installable DnfPackage where
  installer = dnfInstall
  upgrader  = dnfInstall
  getter (DnfPackage (Package n _)) = get n
  toPackage (DnfPackage p) = p

instance MultiInstallable DnfPackage where
  multiInstaller [] = return ()
  multiInstaller pkgs = do
    let latests = filter (\(DnfPackage p) -> version p == Latest) pkgs
    rest <- filterM (\x -> isNothing <$> isInstalled x) (pkgs \\ latests)
    dnfInstallMult $ latests `union` rest

instance Gettable PackageName DnfPackage where
  get pn = do
    (exit, stdout, _stderr) <-
      rpmQuery ["--query", "--queryformat", "%{VERSION}", pn]
    case exit of
      ExitFailure _ -> return Nothing
      ExitSuccess ->
        return . Just $ DnfPackage $ Package { name = pn
                                             , version = Version stdout
                                             }

remove :: DnfPackage -> Craft ()
remove (DnfPackage Package{..}) =
  dnf ["remove", name]

instance Uninstallable DnfPackage where
  uninstaller p = remove p

instance Watchable DnfPackage where
  watch _ _ = notImplemented "Dnf.watch"
