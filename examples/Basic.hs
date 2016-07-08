{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Control.Monad.Logger (runStdoutLoggingT)

import           Craft
import           Craft.Apt (apt)
import qualified Craft.Apt as Apt
import qualified Craft.File as File
import           Craft.Hostname (Hostname(..))
import qualified Craft.Pip as Pip
import           Craft.Run.Vagrant
import           Craft.SSH
import           Craft.SSH.PublicKey
import           Craft.SSH.AuthorizedKey
import qualified Craft.User as User


main :: IO ()
main =
  runStdoutLoggingT $
    runCraftVagrant vagrantSettings {vagrantUp = True} (craftEnv apt) $ do
      craft_ $ Hostname "craft-example-basic"
      void $ Apt.craftPackages packages
      Pip.setup
      craft_ pipPackages
      void craftAdmins


packages :: [Package]
packages =
  map package
    [ "build-essential"
    , "curl"
    , "dnsutils"
    , "git"
    , "htop"
    , "ntp"
    , "sysstat"
    , "tmux"
    , "traceroute"
    , "vim"
    , "wget"
    , "whois"
    , "zip"
    ]


pipPackages :: [Pip.PipPackage]
pipPackages =
  map Pip.latest
    [ "httpie"
    , "requests"
    ]

bash,zsh :: FilePath
bash = "/bin/bash"
zsh = "/usr/bin/zsh"


craftAdmins :: Craft [User]
craftAdmins = sequence
  [ craftAdmin "bob"
               "Bob"
               (publicKey RSA "AAAAB3NzaC1yc2EAAAADAQABAAABAQDVG0Ouvaf1gLUL7i/bH2er5NASDYUIUYuuyuiyasidyqwlejU879as4l00x14ikzbV2a5KqVkluUuBevPqzbYsScK5m5zq1vnlwE2vub8/+6deoNtZ/0NmydSpkadmQmzzotPIC8gGf0O6Y+N39NnJz+XVJmPWY+7mzGWEc39i7p9H59RFuBZMeb7zoRMDquAeLmBQIs3JK95SahKd1zrs1/uWFrGNeKaK1W5DM4rhh4v3/4GhHPozMmCO4wqjcm/HihfI2b+j7V8RIflyFReGPQQA/uxS2OAbkSv3Ax2xj0OE/THr8IWbOEcZwStob+Hh8UZw0iw/kk5mdULxV1t")
               zsh
  , craftAdmin "joehillen"
               "Joe Hillenbrand"
               (publicKey RSA "AAAAB3NzaC1yc2EAAAABIwAAAQEAsiXC83YFBEt586IbjxVILppFEvSS5BIbEnUXMJvFrXC4viG06zWOn/BAUdjGcuz6BSgjHXQkiZ3wks+Y5/AdMh1R4kBs4Gem75qlgMiaYUdUC82Jh3CJ28SnEELt0KJP0LZ+RKRHQhC7NKBGBMFIpdm3C0p2odpMRVRBIOsVW1h9T10mUU5jH59XaunCCB+asxwZMuW11qQH2/oFe17kAFdXRCCSKHDm+4R40NpLrl2KCSQ2ZzNknIBZ8z7YBo16yA637x9lQk9wncomJ4d1HE+F5eW6McbV+zIhthoKJpX4VHsbhA5uYYhk1YS32jcpaSMHRB9cswdFqPzZ833NsQ==")
               bash
  ]


craftNormalUser :: String
                -> String -- Full Name
                -> PublicKey
                -> FilePath
                -> Craft User
craftNormalUser name fullname sshPubKey userShell' = do
  -- we only need to install zsh if someone is using it.
  when (userShell' == zsh) $ craft_ $ package "zsh"
  -- create the user
  user <- craft $ User.userOptions name
                  & User.optComment    .~ fullname
                  & User.optCreateHome .~ False
                  & User.optShell      .~ Just userShell'
  -- create the user's home directory
  craft_ $ directory (user ^. userHome) & ownerAndGroup .~ user
  -- add the user's public key
  craft_ $ AuthorizedKey user sshPubKey
  -- if the user's shell is bash,
  -- then install the standard bashrc
  when (userShell' == bash) $ do
    let bashrcFP = (user ^. userHome)</>".bashrc"
    unlessM (File.exists bashrcFP) $ exec_ "cp" ["/etc/bash.bashrc", bashrcFP]
    craft_ $ file bashrcFP & ownerAndGroup .~ user
  return user


craftAdmin :: String
           -> String -- Full Name
           -> PublicKey
           -> FilePath
           -> Craft User
craftAdmin name fullname sshPubKey userShell' = do
  user <- craftNormalUser name fullname sshPubKey userShell'
  -- add the user to sudoers
  craft_ $ file ("/etc/sudoers.d"</>"10_"++name)
           & strContent .~ name++" ALL=(ALL) NOPASSWD: ALL\n"
  return user
