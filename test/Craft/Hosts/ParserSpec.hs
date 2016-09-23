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
      |127.0.0.1       foobar
      |::1             foobar
      |
      |8.8.8.8         google.com # google.it
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
        Hosts [ (IP "127.0.0.1", [Name "foobar"])
              , (IP "::1",       [Name "foobar"])
              , (IP "8.8.8.8",   [Name "google.com"])
              ]
