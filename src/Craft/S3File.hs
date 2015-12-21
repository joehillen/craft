module Craft.S3File where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File

import           Data.Maybe

data S3File
  = S3File
    { file    :: File
    , source  :: String
    , domain  :: String
    , version :: Version
    }
    deriving (Eq, Show)


s3file :: FilePath -> String -> S3File
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
