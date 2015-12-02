{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe

import           Craft
import           Craft.Apt (Apt(..))
import qualified Craft.Apt as Apt
import           Craft.Directory (Directory(..))
import           Craft.File (File(..), file)
import qualified Craft.File as File
import           Craft.File.Mode
import qualified Craft.Ssh as Ssh
import           Craft.User (User(..), createUser)
import qualified Craft.User as User
import qualified Craft.Group as Group
import qualified Craft.Pip as Pip
import           Craft.Run.Vagrant


main :: IO ()
main =
  runCraftVagrant
    (craftEnv { craftPackageManager = Apt
              }) $ do
      Apt.update
      Pip.setup
      void addAdmins
      packages


bash,zsh :: FilePath
bash = "/bin/bash"
zsh = "/usr/bin/zsh"


addAdmins :: Craft [User]
addAdmins = sequence
  [ admin "bob"
          "Bob"
          "AAAAB3NzaC1yc2EAAAADAQABAAABAQDVG0Ouvaf1gLUL7i/bH2er5NASDYUIUYuuyuiyasidy qwlejU879as4l00x14ikzbV2a5KqVkluUuBevPqzbYsScK5m5zq1vnlwE2vub8/+6deoNtZ/0NmydSpkadmQmzzotPIC8gGf0O6Y+N39NnJz+XVJmPWY+7mzGWEc39i7p9H59RFuBZMeb7zoRMDquAeLmBQIs3JK95SahKd1zrs1/uWFrGNeKaK1W5DM4rhh4v3/4GhHPozMmCO4wqjcm/HihfI2b+j7V8RIflyFReGPQQA/uxS2OAbkSv3Ax2xj0OE/THr8IWbOEcZwStob+Hh8UZw0iw/kk5mdULxV1t"

          userOpts { optShell = zsh }

  , admin "joehillen"
          "Joe Hillenbrand"
          "AAAAB3NzaC1yca8s9dyuna89sDU9A8sdu9a8sUDN9A8sudn9a8SDA9S8Dn aEnUXMJvFrXC4viG06zWOn/BAUdjGcuz6BSgjHXQkiZ3wks+Y5/AdMh1R4kBs4Gem75qlgMiaYUdUC82Jh3CJ28SnEELt0KJP0LZ+RKRHQhC7NKBGBMFIpdm3C0p2odpMRVRBIOsVW1h9T10mUU5jH59XaunCCB+asxwZMuW11qQH2/oFe17kAFdXRCCSKHDm+4R40NpLrl2KCSQ2ZzNknIBZ8z7YBo16yA637x9lQk9wncomJ4d1HE+F5eW6McbV+zIhthoKJpX4VHsbhA5uYYhk1YS32jcpaSMHRB9cswdFqPzZ833NsQ=="
          userOpts
  ]


data UserOptions =
  UserOptions
  { optShell         :: FilePath
  , optSshPubKeyType :: String
  }


userOpts :: UserOptions
userOpts =
  UserOptions
  { optShell = bash
  , optSshPubKeyType = "ssh-rsa"
  }


normalUser :: User.Name
           -> String -- Full Name
           -> String -- SSH Public Key
           -> UserOptions
           -> Craft User
normalUser name fullname sshPubKey UserOptions{..} = do

  -- if the user's shell is zsh
  -- install the zsh package
  when (optShell == zsh) $
    craft_ $ package "zsh"

  -- create the user
  let homepath = "/home" </> name
  user <- createUser name User.opts { User.optComment    = fullname
                                    , User.optUserGroup  = True
                                    , User.optCreateHome = False
                                    , User.optHome       = Just homepath
                                    , User.optShell      = Just optShell
                                    }

  -- create the user's home directory
  craft_ $ Directory homepath
                     (Mode RWX RX RX)
                     (User.uid user)
                     (Group.gid $ User.group user)

  -- add the user's public key
  void $ Ssh.addAuthorizedKey user $ Ssh.PublicKey sshPubKey optSshPubKeyType

  -- if the user's shell is bash
  -- then instal the standard bashrc
  when (optShell == bash) $ do
    bashrc <- File.content . fromJust <$> File.get "/etc/bash.bashrc"
    craft_ $
      File (homepath </> ".bashrc")
           (Mode RW R R)
           (User.uid user)
           (Group.gid $ User.group user)
           bashrc

  return user


admin :: User.Name
      -> String -- Full Name
      -> String -- SSH Public Key
      -> UserOptions
      -> Craft User
admin name fullname sshPubKey opts = do
  user <- normalUser name fullname sshPubKey opts

  -- add the user to sudo
  craft_ $
    (file $ "/etc/sudoers.d" </> "10_" ++ name)
      { content = Just . B8.pack $ name ++ " ALL=(ALL) NOPASSWD: ALL" }

  return user


packages :: Craft ()
packages = do
  aptPackages
  pipPackages


aptPackages :: Craft ()
aptPackages =
  mapM_ (craft . package)
    [ "autoconf"
    , "automake"
    , "build-essential"
    , "curl"
    , "djbdns"
    , "emacs24-nox"
    , "git"
    , "htop"
    , "netemul"
    , "ntp"
    , "pv"
    , "python-pip"
    , "sysstat"
    , "tmux"
    , "traceroute"
    , "wget"
    , "xfsprogs"
    , "zip"
    , "zsh"
    , "dnsutils"
    , "whois"
    ]


pipPackages :: Craft ()
pipPackages =
  mapM_ (craft . Pip.latest)
    [ "httpie"
    , "requests"
    ]
