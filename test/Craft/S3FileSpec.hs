module Craft.S3FileSpec (spec) where

import           Data.List             (intercalate)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified Text.Megaparsec       as MP

import           Craft.S3File

respCRLF :: String
respCRLF = intercalate "\r\n" $ status:headerLines

respNL :: String
respNL = unlines $ status:headerLines

headerLines :: [String]
headerLines = map (\(k,v)->k++": "++v) expected

status :: String
status = "HTTP/1.1 200 OK"


expected :: [(String, String)]
expected =
  [ ("x-amz-id-2", "3PEnvyK2lmZYO2eOcG31YV7cMtrIHh4LVkIHma4vTLUSlTp2MHWYVytfbv6KwtDcfpjv9mKmdZE=")
  , ("x-amz-request-id", "534CFEB94E255448")
  , ("Date", "Wed, 13 Jul 2016 21:08:41 GMT")
  , ("Last-Modified", "Wed, 13 Jul 2016 21:04:12 GMT")
  , ("ETag", "\"0d329bab9fe82cae99a6777767efc144\"")
  , ("x-amz-storage-class", "REDUCED_REDUNDANCY")
  , ("Accept-Ranges", "bytes")
  , ("Content-Type", "binary/octet-stream")
  , ("Content-Length", "29323142")
  , ("Server", "AmazonS3")
  ]


spec :: Spec
spec = do
  describe "httpHeaders" $ do
    let parse = MP.parse httpHeaders ""
    it "parses response separated by newlines" $
      parse respNL `shouldParse` expected
    it "parses response with trailing newline" $
      parse (respNL++"\n") `shouldParse` expected
    it "parses response separated by crlf" $
      parse respCRLF `shouldParse` expected
    it "parses response with trailing crlf" $
      parse (respCRLF++"\r\n") `shouldParse` expected
