{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 711
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
module Craft.S3File where

import           Control.Lens            hiding (noneOf)
import           Control.Monad.Logger    (logDebug)
import           Craft.Checksum
import qualified Crypto.Hash             as Crypto
import           Crypto.MAC.HMAC         (HMAC, hmac)
import           Data.ByteArray.Encoding
import qualified Data.ByteString.Char8   as B8
import qualified Data.Text               as T
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft                   hiding (try)
import qualified Craft.File              as File


data S3File
  = S3File
    { _file      :: File
    , _source      :: String
    , _domain      :: String
    , _fileVersion :: Version MD5
    , _auth        :: Maybe (String, String) -- ^ (AWSAccessKeyId, AWSSecretAccessKey)
    }
    deriving (Eq, Show)

makeLenses ''S3File

instance FileLike S3File where
  type FileLikePath S3File = AbsFilePath
  path = Craft.S3File.file . filePath
  mode = Craft.S3File.file . fileMode
  ownerID = Craft.S3File.file . fileOwnerID
  groupID = Craft.S3File.file . fileGroupID


s3file :: AbsFilePath -> String -> S3File
s3file fp source' =
  S3File
  { _file      = Craft.file fp
  , _domain    = "s3.amazonaws.com"
  , _source    = source'
  , _fileVersion = AnyVersion
  , _auth      = Nothing
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
      date <- $stdoutOrError =<< exec "date" ["-u", "--rfc-2822"]
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
          sigHMAC :: HMAC Crypto.SHA1
          sigHMAC = hmac (B8.pack awsSecretKey)
                         (B8.pack toSign)
          sig = B8.unpack $ convertToBase Base64 sigHMAC
      $logDebug . T.pack $ "awsSecretKey == " ++ awsSecretKey
      $logDebug . T.pack $ "awsKeyID == " ++ awsKeyID
      $logDebug . T.pack $ "toSign == " ++ show toSign

      return [ "--header", "Date:" ++ date
             , "--header", "Authorization:AWS " ++ awsKeyID ++ ":" ++ sig
             ]


getS3Sum :: S3File -> Craft (Maybe MD5)
getS3Sum f = do
  hdrs <- authHeaders "HEAD" f
  headers <- parseExecStdout httpHeaders "curl" (hdrs ++ ["-s", "-XHEAD", "-I", f ^. url])
  return $ MD5 <$> filter ('"' /=) <$> lookup "ETag" headers


httpHeaders :: Parser [(String, String)]
httpHeaders = do
  _ <- string "HTTP/1." >> oneOf ['0', '1'] >> string " 200 OK" >> eol
  headers <- header `sepEndBy1` eol
  return headers


header :: Parser (String, String)
header = do
  k <- noneOf [':', '\r', '\n'] `someTill` string ": "
  v <- many $ noneOf ['\r', '\n']
  return (k,v)


instance Craftable S3File S3File where
  watchCraft s3f = do
    let s3f' = s3f & Craft.S3File.file . fileContent .~ Nothing
    let fp   = s3f' ^. Craft.S3File.file . path
    let downloadFile = do
          hdrs <- authHeaders "GET" s3f
          exec_ "curl" $ hdrs ++ ["-XGET", "-s", "-L", "-o", fromAbsFile fp, s3f'^.url]
    let verify expected =
          check (s3f'^.Craft.S3File.file) expected >>= \case
            Matched -> return ()
            ChecksumFailed r -> $craftError $ "Checksum failed: "++show r
            Mismatched a -> $craftError $ "Wrong Version: "++show a++"Expected: "++show expected
    getS3Sum s3f' >>= \case
      Nothing -> $craftError $ "Failed to get chksum from S3 for: " ++ show s3f'
      Just etag -> do
        exists <- File.exists fp
        w <-
          if exists
          then do
            curSum <- MD5 <$> File.md5sum fp
            case s3f'^.fileVersion of
              AnyVersion -> return Unchanged
              Latest
                | etag == curSum -> return Unchanged
                | otherwise -> do
                    downloadFile
                    verify etag
                    return Updated
              ExactVersion md5sum
                | curSum == md5sum -> return Unchanged
                | md5sum /= etag ->
                    $craftError $
                       "Cannot download specific file version from S3. "
                       ++ "Found version " ++ show etag ++ " for " ++ show s3f'
                | otherwise -> do
                    downloadFile
                    verify md5sum
                    return Updated
          else do
            downloadFile
            case s3f'^.fileVersion of
              ExactVersion ver -> verify ver
              _                -> verify etag
            return Created

        fw <- watchCraft_ $ s3f' ^. Craft.S3File.file
        if changed w then
          return (w, s3f')
        else
          return (fw, s3f')
