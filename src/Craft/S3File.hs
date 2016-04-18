module Craft.S3File where

import           Control.Lens hiding (noneOf)
import           Control.Monad.Logger (logDebug)
import           Crypto.Hash (SHA1)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Data.ByteArray.Encoding
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft
import           Craft.File (File)
import qualified Craft.File as File


data S3File
  = S3File
    { _file    :: File
    , _source  :: String
    , _domain  :: String
    , _version :: Version
    , _auth    :: Maybe (String, String) -- ^ (AWSAccessKeyId, AWSSecretAccessKey)
    }
    deriving (Eq, Show)

makeLenses ''S3File


s3file :: FilePath -> String -> S3File
s3file fp source' =
  S3File
  { _file    = File.file fp
  , _domain  = "s3.amazonaws.com"
  , _source  = source'
  , _version = AnyVersion
  , _auth    = Nothing
  }


url :: Getter S3File String
url = to (\f -> "https://" ++ f ^. domain ++ "/" ++ f ^. source)


-- | Add AWS Authentication Headers to curl commands
-- http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#ConstructingTheAuthenticationHeader
authHeaders :: String -> S3File -> Craft [String]
authHeaders method s3f =
  case s3f ^. auth of
    Nothing -> return []
    Just (awsKeyID, awsSecretKey) -> do
      date <- view stdout <$> ($errorOnFail =<< exec "date" ["-u", "--rfc-2822"])
      sourceUrl <- case s3f ^. source of
                     []       -> $craftError $ "S3File.source is empty! " ++ show s3f
                     '/':_    -> return $ s3f ^. source
                     s3source -> return $ '/':s3source
      let contentMD5 = ""
          contentType = ""
          canonicalizedAmzHeaders = ""
          toSign = method ++ "\n"
                ++ contentMD5 ++ "\n"
                ++ contentType ++ "\n"
                ++ date ++ "\n"
                ++ canonicalizedAmzHeaders
                ++ sourceUrl
          sigHMAC :: HMAC SHA1
          sigHMAC = hmac (B8.pack awsSecretKey)
                         (B8.pack toSign)
          sig = B8.unpack $ convertToBase Base64 sigHMAC
      $logDebug . T.pack $ "awsSecretKey == " ++ awsSecretKey
      $logDebug . T.pack $ "awsKeyID == " ++ awsKeyID
      $logDebug . T.pack $ "toSign == " ++ show toSign

      return [ "--header", "Date:" ++ date
             , "--header", "Authorization:AWS " ++ awsKeyID ++ ":" ++ sig
             ]

-- auth_header = "Authorization:AWS "+AWSAccessKeyId+":"+sig
-- date_header = "Date:"+ amazons3time

getS3Sum :: S3File -> Craft (Maybe String)
getS3Sum f = do
  hdrs <- authHeaders "HEAD" f
  r <- $errorOnFail =<< exec "curl" (hdrs ++ ["-XHEAD", "-I", f ^. url])
  headers <- parseExecResult (ExecSucc r) httpHeaders $ r ^. stdout
  return $ filter ('"' /=) <$> lookup "ETag" headers


httpHeaders :: Parser [(String, String)]
httpHeaders = do
  _ <- string "HTTP/1." >> oneOf "01" >> string " 200 OK" >> eol
  some header


header :: Parser (String, String)
header = (,) <$> noneOf ":" `someTill` try (string ": ")
             <*> anyChar `manyTill` try (skipSome eol <|> eof)


instance Craftable S3File where
  watchCraft s3f = do
    let s3f' = s3f & file . File.content .~ Nothing
    let fp = s3f' ^. file . File.path
    let downloadFile = do
          hdrs <- authHeaders "GET" s3f
          exec_ "curl" $ hdrs ++ ["-XGET", "-s", "-L", "-o", fp, s3f' ^. url]
    let verify expected = do
          md5chksum <- File.md5sum fp
          when (md5chksum /= expected) (
            $craftError $ "verify S3File failed! Expected `" ++ expected ++ "` "
                       ++ "Got `" ++ md5chksum ++ "` for " ++ show s3f')

    getS3Sum s3f' >>= \case
      Nothing -> $craftError $ "Failed to get chksum from S3 for: " ++ show s3f'
      Just etag -> do
        exists <- File.exists fp
        w <- if exists then do
                curSum <- File.md5sum fp
                case s3f' ^. version of
                  AnyVersion -> return Unchanged
                  Latest
                    | etag == curSum -> return Unchanged
                    | otherwise -> do
                        downloadFile
                        verify etag
                        return Updated
                  Version verStr
                    | curSum == verStr -> return Unchanged
                    | verStr /= etag ->
                        $craftError $
                           "Cannot download specific file version from S3. "
                           ++ "Found version " ++ etag ++ " for " ++ show s3f'
                    | otherwise -> do
                        downloadFile
                        verify verStr
                        return Updated
              else do
                downloadFile
                case s3f' ^. version of
                  Version verStr -> verify verStr
                  _              -> verify etag
                return Created

        fw <- watchCraft_ $ s3f' ^. file
        if changed w then
          return (w, s3f')
        else
          return (fw, s3f')
