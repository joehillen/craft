module Craft.S3File where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Internal.FileDirectory
import           Craft.Internal.Helpers

import           Control.Lens
import           Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Network.Wreq (head_, responseHeader)

data S3File
  = S3File
    { file    :: File
    , source  :: String
    , domain  :: String
    , version :: Version
    }
    deriving (Eq, Show)

s3file :: File.Path -> String -> S3File
s3file path source =
  S3File
  { file    = File.file path
  , domain  = "s3.amazonaws.com"
  , source  = source
  , version = AnyVersion
  }

getS3Sum :: String -> Craft (Maybe String)
getS3Sum = notImplemented "getS3Sum"

-- getS3Sum :: String -> IO (Maybe String)
-- getS3Sum url = do
--   r <- head_ url
--   return $ filter ('"' /=) . BS.unpack <$> (r ^? responseHeader "ETag")


instance Craftable S3File where
  checker s3f = checker (file s3f) >>= \case
    Nothing -> return Nothing
    Just  f -> do
      v <- File.md5sum $ File.path f
      return . Just $
        s3f { file    = f
            , version = Version v
            }

  crafter s3f@S3File{..} = do
    let path = File.path file
    let url = "https://" ++ domain ++ "/" ++ source
    let downloadFile = do
          craft_ $ package "curl"
          exec_ "/usr/bin/curl" ["-s", "-L", "-o", path, url]

    exists <- File.exists path
    curSum <- File.md5sum path
    s3Sum <- fromJust <$> getS3Sum url

    case version of
      AnyVersion     -> unless  exists                      downloadFile
      Latest         -> unless (exists && s3Sum == curSum)  downloadFile
      Version verStr -> unless (exists && curSum == verStr) downloadFile

    setMode (File.mode file) path
    whenJust (File.owner file) $ setOwner path
    whenJust (File.group file) $ setGroup path

  remover S3File{..} = remover file
