module Craft.Ubuntu where

import           Control.Lens
import           Control.Monad.Logger (logDebug)
import           Data.ByteString.Lens (unpackedChars)
import           Data.Maybe           (catMaybes)
import           Data.String.Utils    (replace)
import qualified Data.Text            as T
import           Formatting

import           Craft
import           Craft.Apt            as Apt
import qualified Craft.File           as File


data PPA = PPA { _ppaURL :: String }
  deriving (Eq, Show)
makeLenses ''PPA


findPPAFiles :: PPA -> Craft [File]
findPPAFiles (PPA url) = do
  mbfs <-
    mapM (File.getWithContent . view path)
    =<< File.find
          $(mkAbsDir "/etc/apt/sources.list.d")
          ["-name", "*" ++ replace "/" "*" url ++ "*.list"]
  let fs = catMaybes mbfs
  let nonEmpty = (> 0) . length . view (fileContent . _Just . unpackedChars)
  let r = filter nonEmpty fs
  $logDebug . T.pack $ "findPPAFiles found: " ++ show (map (fromAbsFile . view path) r)
  return r


instance Craftable PPA PPA where
  watchCraft ppa@(PPA url) = do
    fs <- findPPAFiles ppa
    if null fs
    then do
      craft_ $ package "software-properties-common"
      exec_ "add-apt-repository" ["-y", "ppa:" ++ url]
      Apt.update
      return (Created, ppa)
    else return (Unchanged, ppa)


instance Destroyable PPA where
  watchDestroy ppa@(PPA url) = do
    fsBefore <- findPPAFiles ppa
    if null fsBefore
    then return (Unchanged, Nothing)
    else do
      craft_ $ package "software-properties-common"
      exec_ "add-apt-repository" ["-y", "-r", "ppa:" ++ url]
      fsAfter <- findPPAFiles ppa
      unless (null fsAfter) $
        $craftError $ formatToString ("destroy PPA `"%shown%"` failed! Found: "%shown) ppa fsAfter
      Apt.update
      return (Removed, Just ppa)
