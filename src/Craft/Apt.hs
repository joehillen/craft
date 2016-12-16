module Craft.Apt where

import           Control.Lens
import           Control.Monad
import           Formatting

import           Craft
import qualified Craft.Package as Package



apt :: PackageManager
apt =
  PackageManager
  { _pmName           = "apt"
  , _pmGetter         = getAptPackage
  , _pmInstaller      = aptInstall
  , _pmUpgrader       = aptInstall
  , _pmUninstaller    = aptPurge
  }


aptGet :: [String] -> Craft ()
aptGet args = exec_ "apt-get" $ ["-q", "-y"] ++ args


update :: Craft ()
update = aptGet ["update"]


autoremove :: Craft ()
autoremove = aptGet ["autoremove"]


dpkgQuery :: [String] -> Craft ExecResult
dpkgQuery = exec dpkgQueryBin


dpkgQueryBin :: String
dpkgQueryBin = "dpkg-query"


dpkgQueryStatus :: String -> Craft ExecResult
dpkgQueryStatus pn = dpkgQuery ["-s", pn]


expectOutput :: String -> [String] -> Craft String
expectOutput cmd args = do
  r <- $stdoutOrError =<< exec cmd args
  when (null r) $ $craftError $ formatToString ("'"%string%"' returned an empty result!")
                                               (unwords $ cmd:args)
  return r


dpkgQueryShow :: String -> String -> Craft String
dpkgQueryShow pattern n = expectOutput dpkgQueryBin [ "--show", "--showformat", pattern, n ]


dpkgQueryVersion :: String -> Craft String
dpkgQueryVersion = dpkgQueryShow "${Version}"


dpkgQueryPackage :: String -> Craft String
dpkgQueryPackage = dpkgQueryShow "${Package}"


getAptPackage :: PackageName -> Craft (Maybe Package)
getAptPackage pn =
  dpkgQueryStatus pn >>= \case
    Failure _ -> return Nothing
    Success _ -> Just <$> do
      r <- dpkgQueryVersion pn
      return $ Package pn (Version r)


aptInstallArgs :: [String]
aptInstallArgs = ["-o", "DPkg::Options::=--force-confnew", "install"]


aptInstall :: Package -> Craft ()
aptInstall pkg = aptGet $ aptInstallArgs ++ [pkgArg pkg]


aptInstallMult :: [Package] -> Craft ()
aptInstallMult []   = return ()
aptInstallMult pkgs = aptGet $ aptInstallArgs ++ map pkgArg pkgs


pkgArg :: Package -> String
pkgArg (Package pn AnyVersion)  = pn
pkgArg (Package pn Latest)      = pn
pkgArg (Package pn (Version v)) = pn ++ "=" ++ v


aptRemove :: Package -> Craft ()
aptRemove Package{..} = aptGet ["remove", _pkgName]


aptPurge :: Package -> Craft ()
aptPurge Package{..} = aptGet ["remove", _pkgName, "--purge"]


data Deb = Deb
  { _debFile :: File
  , _debPkg  :: Package
  }
  deriving (Eq, Show)
makeLenses ''Deb


debName :: Lens' Deb String
debName = debPkg . pkgName


debVersion :: Lens' Deb (Version a)
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
  withEnvVar "DEBIAN_FRONTEND" "noninteractive" $
    exec_ "dpkg"
      [ "--force-overwrite"
      , "--force-confnew"
      , "-i", fromAbsFile $ f^.path
      ]


dpkgDebBin :: String
dpkgDebBin = "dpkg-deb"


dpkgDebShow :: String -> File -> Craft String
dpkgDebShow pattern f =
  expectOutput dpkgDebBin ["--show", "--showformat", pattern, fromAbsFile $ f^.path]


dpkgDebVersion :: File -> Craft String
dpkgDebVersion = dpkgDebShow "${Version}"


dpkgDebPackage :: File -> Craft String
dpkgDebPackage = dpkgDebShow "${Package}"


instance Craftable Deb Deb where
  watchCraft d = do
    let name = d ^. debName
    let expectedVersion = d ^. debVersion
    let get = getAptPackage name
    let error' = formatToString ("craft Deb `"%shown%"` failed! "%string) d
    let installAndVerify = do
          dpkgInstall $ d ^. debFile
          get >>= \case
            Nothing -> $craftError $ error' "Package Not Found."
            Just installedPkg ->
              when (expectedVersion /= installedPkg ^. pkgVersion) $
                $craftError . error' $ formatToString ("Wrong Version: "%shown%" Expected: "%shown) (installedPkg ^. pkgVersion) expectedVersion
    get >>= \case
      Nothing  -> do
        installAndVerify
        return (Created, d)
      Just installedPkg ->
        if expectedVersion == installedPkg ^. pkgVersion
        then return (Unchanged, d)
        else do
          installAndVerify
          return (Updated, d)


-- |A function that installs a list of Apt packages,
-- |but that only runs "apt-get update" if a given package is not already installed.
-- TESTME
craftPackages :: [Package] -> Craft [Package]
craftPackages packages = do
  whenM ((any comparePackages . zip packages) <$> mapM (Package.get . _pkgName) packages)
    update
  craft packages
 where
  comparePackages (_, Nothing) = True
  comparePackages (Package _ expectedVersion, Just (Package _ actualVersion)) =
    case (expectedVersion, actualVersion) of
      (AnyVersion, _) -> False
      (Latest,     _) -> True
      (x,          y) -> x /= y


craftUnattendedUpgrades :: Craft ()
craftUnattendedUpgrades = do
  craft_ $ package "unattended-upgrades"
  craft_ $
    file $(mkAbsFile "/etc/apt/apt.conf.d/20auto-upgrades")
    & strContent .~ unlines
      [ "APT::Periodic::Enable \"1\";"
      , "APT::Periodic::Update-Package-Lists \"1\";"
      , "APT::Periodic::Unattended-Upgrade \"1\";"
      , "APT::Periodic::Download-Upgradeable-Packages \"1\";"
      , "APT::Periodic::AutocleanInterval \"7\";"
      ]
  w <- watchCraft_ $
    file $(mkAbsFile "/etc/apt/apt.conf.d/50unattended-upgrades")
    & strContent .~ unlines
      [ "Unattended-Upgrade::Origins-Pattern {"
      , "  \"o=*\";"
      , "};"
      , "Unattended-Upgrade::Remove-Unused-Dependencies \"true\";"
      ]
  when (changed w) $
    exec_ "unattended-upgrades" ["-v"]
