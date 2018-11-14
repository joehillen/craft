module Craft.Wget where

import           Control.Lens
import           Data.Monoid    ((<>))

import           Craft
import           Craft.Checksum (Checksum)
import qualified Craft.Checksum as Checksum
import qualified Craft.File     as File

data Wget = Wget
  { _url    :: String
  , _dest   :: File
  , _chksum :: Maybe Checksum
  , _args   :: Args -- ^Additional arguments to add to the wget command, such as timeouts.
  }
  deriving (Show, Eq)
makeLenses ''Wget


wget :: String -> AbsFilePath -> Wget
wget url' destfp =
  Wget
  { _url    = url'
  , _dest   = file destfp
  , _chksum = Nothing
  , _args   = []
  }


instance FileLike Wget where
  type FileLikePath Wget = AbsFilePath
  path = dest . filePath
  mode = dest . fileMode
  ownerID = dest . fileOwnerID
  groupID = dest . fileGroupID


instance Craftable Wget Wget where
  watchCraft wg = do
    let destf  = wg ^. dest & fileContent .~ Nothing
    let destfp = wg ^. dest . path
    let mismatchError actual = "Checksum Mismatch for `"<>wg^.url<>"`. "
                            <> "Expected `"<>show (wg^.chksum)<>"` "
                            <> "Got `"<>show actual<>"`"
    let check = Checksum.check destf
    let watchCraftDest = do
          w <- watchCraft_ destf
          return (w, wg)

    let freshDownload = do
          run wg
          case wg ^. chksum of
            Nothing      -> watchCraftDest
            Just chksum' ->
              check chksum' >>= \case
                Checksum.Matched           -> watchCraftDest
                Checksum.Failed r          -> $craftError $ show r
                Checksum.Mismatched actual -> $craftError $ mismatchError actual
    File.exists destfp >>= \case
      True ->
        case wg ^. chksum of
          Nothing      -> watchCraftDest
          Just chksum' ->
            check chksum' >>= \case
              Checksum.Matched      -> watchCraftDest
              Checksum.Failed r     -> $craftError $ show r
              Checksum.Mismatched _ -> freshDownload
      False -> do
        void freshDownload
        return (Created, wg)


run :: Wget -> Craft ()
run wg =
  (exec_ "wget" $ (wg^.args) ++ ["-O", (fromAbsFile $ wg^.dest.path), wg^.url])
  `onException`
    (destroy_ $ wg^.dest.path)
