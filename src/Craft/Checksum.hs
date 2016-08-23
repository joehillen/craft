module Craft.Checksum where

import           Control.Lens

import           Craft

data Checksum
  = CRC    String
  | MD5    String
  | SHA1   String
  | SHA224 String
  | SHA256 String
  | SHA384 String
  | SHA512 String
  deriving (Show, Eq)


command :: Checksum -> String
command (CRC    _) = "cksum"
command (MD5    _) = "md5sum"
command (SHA1   _) = "sha1sum"
command (SHA224 _) = "sha224sum"
command (SHA256 _) = "sha256sum"
command (SHA384 _) = "sha384sum"
command (SHA512 _) = "sha512sum"


toString :: Checksum -> String
toString (CRC    x) = x
toString (MD5    x) = x
toString (SHA1   x) = x
toString (SHA224 x) = x
toString (SHA256 x) = x
toString (SHA384 x) = x
toString (SHA512 x) = x


data Result
  = Failed FailResult
  | Matched
  | Mismatched Checksum
  deriving (Show)


-- | Check a 'File' against the given 'Checksum'.
-- If the 'File' does not exist or the checksum command fails,
-- 'Failed' is returned.
-- If 'Checksum' does not match,
-- 'Mismatched' is returned containing the actual 'Checksum'.
-- Otherwise 'Matched' is returned.
check :: File -> Checksum -> Craft Result
check f chksum = exec (command chksum) [fromAbsFile $ f^.path] >>= \case
  ExecFail r -> return $ Failed r
  ExecSucc r -> return $
    let mk = case chksum of
               CRC    _ -> CRC
               MD5    _ -> MD5
               SHA1   _ -> SHA1
               SHA224 _ -> SHA224
               SHA256 _ -> SHA256
               SHA384 _ -> SHA384
               SHA512 _ -> SHA512
        actual = mk $ r ^. stdout
    in if chksum == actual
          then Mismatched actual
          else Matched
