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
dpkgQuery = exec dpkgQueryBin


dpkgQueryBin :: String
dpkgQueryBin = "/usr/bin/dpkg-query"


dpkgQueryStatus :: String -> Craft ExecResult
dpkgQueryStatus pn = dpkgQuery ["-s", pn]


dpkgQueryShow :: String -> String -> Craft String
dpkgQueryShow pattern n = do
  let args = [ "--show", "--showformat", pattern, n ]
  r <- stdout . errorOnFail
         <$> dpkgQuery args
  when (null r) $ error $ "'" ++ (unwords (dpkgQueryBin:args))
                          ++ "' returned an empty result!"
  return r


dpkgQueryVersion :: String -> Craft String
dpkgQueryVersion = dpkgQueryShow "${Version}"


dpkgQueryPackage :: String -> Craft String
dpkgQueryPackage = dpkgQueryShow "${Package}"


getAptPackage :: PackageName -> Craft (Maybe Package)
getAptPackage pn = do
  dpkgQueryStatus pn >>= \case
    ExecFail _ -> return Nothing
    ExecSucc _ -> Just <$> do
      r <- dpkgQueryVersion pn
      return $ Package { pkgName = pn
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
dpkgDebShow pattern f = do
  let args = [ "--show", "--showformat", pattern, File.path f ]
  r <- stdout . errorOnFail
         <$> exec dpkgDebBin args
  when (null r) $ error $ "'" ++ (unwords (dpkgDebBin:args))
                          ++ "' returned an empty result!"
  return r


dpkgDebVersion :: File -> Craft String
dpkgDebVersion = dpkgDebShow "${Version}"


dpkgDebPackage :: File -> Craft String
dpkgDebPackage = dpkgDebShow "${Package}"


instance Craftable Deb where
  checker (Deb f pkg) = do
    checker pkg >>= \case
      Nothing  -> return Nothing
      Just pkg -> return . Just $ Deb f pkg

  crafter (Deb f pkg) mdeb =
    case mdeb of
      Nothing             -> dpkgInstall f
      Just (Deb _ oldpkg) ->
        when (pkgVersion oldpkg /= pkgVersion pkg) $
          dpkgInstall f

  destroyer deb = notImplemented "destroyer Deb"
