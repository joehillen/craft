module Craft.S3File where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File

import Control.Lens


data S3File
  = S3File
    { _file    :: File
    , _source  :: Text
    , _domain  :: Text
    , _version :: Version
    }
    deriving (Eq, Show)
makeLenses ''S3File


s3file :: FilePath -> Text -> S3File
s3file fp source' =
  S3File
  { _file    = File.file fp
  , _domain  = "s3.amazonaws.com"
  , _source  = source'
  , _version = AnyVersion
  }


getS3Sum :: Text -> Craft (Maybe Text)
getS3Sum url = $notImplemented "getS3Sum"

-- getS3Sum :: Text -> IO (Maybe Text)
-- getS3Sum url = do
--   r <- head_ url
--   return $ filter ('"' /=) . BS.unpack <$> (r ^? responseHeader "ETag")


{-
instance Craftable S3File where
  watchCraft s3f = do
    let fp = File.path $ file s3f
    let url = "https://" ++ domain s3f ++ "/" ++ source s3f
    let downloadFile = exec_ "curl" ["-s", "-L", "-o", fp, url]

    curSum <- File.md5sum fp
    s3Sum <- fromJust <$> getS3Sum url

    unless (isJust s3f) $
      case version of
        AnyVersion     -> downloadFile
        Latest         -> unless (s3Sum == curSum)  downloadFile
        Version verStr -> unless (curSum == verStr) downloadFile

    craft_ $ file { File.content = Nothing }
-}
