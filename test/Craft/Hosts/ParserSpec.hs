{-# LANGUAGE QuasiQuotes #-}
module Craft.Hosts.ParserSpec (spec) where


import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Heredoc
import qualified Text.Megaparsec       as MP

import           Craft.Hosts.Parser
import           Craft.Hosts.Types

hostsfile :: String
hostsfile =
  [str|#
      |# /etc/hosts: static lookup table for host names
      |#
      |
      |#<ip-address>  <hostname.domain.org>  <hostname>
      |127.0.0.1      ipv4.foobar.local foobar
      | ::1            foobar
      |
      | 8.8.8.8         google.com # google.it
      |
      |
      |# End of file
      |]


spec :: Spec
spec = do
  describe "parseHosts" $ do
    let parse = MP.parse parser ""
    it "parses a typical hosts files" $
      parse hostsfile
      `shouldParse`
        Hosts [ (IP "127.0.0.1", [Name "ipv4.foobar.local", Name "foobar"])
              , (IP "::1",       [Name "foobar"])
              , (IP "8.8.8.8",   [Name "google.com"])
              ]
    it "parses an empty file" $
      parse "" `shouldParse` Hosts []
    it "parses only a newline" $
      parse "\n" `shouldParse` Hosts []
    it "parses only a comment" $
      parse "#FOOBAR" `shouldParse` Hosts []
    it "parses only a comment with a newline" $
      parse "#FOOBAR\n" `shouldParse` Hosts []
    it "parses whitespace only" $
      parse "\n  \n\n" `shouldParse` Hosts []
    it "parses only an ipv4 address and domain" $
      parse "0.0.0.0 a" `shouldParse` Hosts [(IP "0.0.0.0", [Name "a"])]
    it "parses only an ipv6 address and domain" $
      parse "23fb:1234::1\ta\n" `shouldParse` Hosts [(IP "23fb:1234::1", [Name "a"])]
