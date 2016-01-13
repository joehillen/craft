module Craft.S3File where

import           Craft
import           Craft.File (File)
import qualified Craft.File as File

import Control.Lens hiding (noneOf)
import Text.Megaparsec
import Text.Megaparsec.String


data S3File
  = S3File
    { _file    :: File
    , _source  :: String
    , _domain  :: String
    , _version :: Version
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
  }


url :: Getter S3File String
url = to (\f -> "https://" ++ f ^. domain ++ "/" ++ f ^. source)



getS3Sum :: S3File -> Craft (Maybe String)
getS3Sum f = do
  r <- $errorOnFail =<< exec "curl" ["-I", f ^. url]
  headers <- parseExecResult (ExecSucc r) httpHeaders $ r ^. stdout
  return $ filter ('"' /=) <$> lookup "ETag" headers


httpHeaders :: Parser [(String, String)]
httpHeaders = do
  _ <- string "HTTP/" >> digitChar >> char '.' >> digitChar >> string " 200 OK" >> eol
  some header


header :: Parser (String, String)
header = do
  name <- (noneOf ":") `someTill` try (string ": ")
  value <- anyChar `manyTill` try (skipSome eol <|> eof)
  return (name, value)


instance Craftable S3File where
  watchCraft s3f = do
    let s3f' = s3f & file . File.content .~ Nothing
    let fp = s3f' ^. file . File.path
    let downloadFile = exec_ "curl" ["-s", "-L", "-o", fp, s3f' ^. url]

    getS3Sum s3f' >>= \case
      Nothing -> $craftError $ "Failed to get chksum from S3 for: " ++ show s3f'
      Just etag -> do
        exists <- File.exists fp
        w <- if exists then do
                curSum <- File.md5sum fp
                case s3f' ^. version of
                  AnyVersion     -> return Unchanged
                  Latest         -> if etag == curSum then
                                      return Unchanged
                                    else do
                                      downloadFile
                                      return Updated
                  Version verStr -> if curSum == verStr then
                                      return Unchanged
                                    else do
                                      downloadFile
                                      return Updated
              else do
                downloadFile
                return Created

        fw <- watchCraft_ $ s3f' ^. file
        if changed w then
          return (w, s3f')
        else
          return (fw, s3f')
