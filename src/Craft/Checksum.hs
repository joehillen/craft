module Craft.Checksum where

import           Control.Lens

import           Craft

class (Show a, Eq a) => Checkable a where
  checksumCommand :: a -> String
  checksumString :: a -> String
  mkChecksum :: String -> a

newtype CRC = CRC String
  deriving (Eq, Show)

instance Checkable CRC where
  checksumCommand (CRC    _) = "cksum"
  checksumString (CRC s) = s
  mkChecksum s = CRC s

newtype MD5 = MD5 String
  deriving (Eq, Show)

instance Checkable MD5 where
  checksumCommand (MD5    _) = "md5sum"
  checksumString (MD5 s) = s
  mkChecksum s = MD5 s

newtype SHA1 = SHA1 String
  deriving (Eq, Show)

instance Checkable SHA1 where
  checksumCommand (SHA1   _) = "sha1sum"
  checksumString (SHA1 s) = s
  mkChecksum s = SHA1 s

newtype SHA224 = SHA224 String
  deriving (Eq, Show)

instance Checkable SHA224 where
  checksumCommand (SHA224 _) = "sha224sum"
  checksumString (SHA224 s) = s
  mkChecksum s = SHA224 s

newtype SHA256 = SHA256 String
  deriving (Eq, Show)

instance Checkable SHA256 where
  checksumCommand (SHA256 _) = "sha256sum"
  checksumString (SHA256 s) = s
  mkChecksum s = SHA256 s

newtype SHA384 = SHA384 String
  deriving (Eq, Show)

instance Checkable SHA384 where
  checksumCommand (SHA384 _) = "sha384sum"
  checksumString (SHA384 s) = s
  mkChecksum s = SHA384 s

newtype SHA512 = SHA512 String
  deriving (Eq, Show)

instance Checkable SHA512 where
  checksumCommand (SHA512 _) = "sha512sum"
  checksumString (SHA512 s) = s
  mkChecksum s = SHA512 s


data Checksum
  = CRCSum CRC
  | MD5Sum MD5
  | SHA1Sum SHA1
  | SHA224Sum SHA224
  | SHA256Sum SHA256
  | SHA384Sum SHA384
  | SHA512Sum SHA512
  deriving (Eq, Show)

instance Checkable Checksum where
  checksumCommand (CRCSum s) = checksumCommand s
  checksumCommand (MD5Sum s) = checksumCommand s
  checksumCommand (SHA1Sum s) = checksumCommand s
  checksumCommand (SHA224Sum s) = checksumCommand s
  checksumCommand (SHA256Sum s) = checksumCommand s
  checksumCommand (SHA384Sum s) = checksumCommand s
  checksumCommand (SHA512Sum s) = checksumCommand s

  checksumString (CRCSum s) = checksumString s
  checksumString (MD5Sum s) = checksumString s
  checksumString (SHA1Sum s) = checksumString s
  checksumString (SHA224Sum s) = checksumString s
  checksumString (SHA256Sum s) = checksumString s
  checksumString (SHA384Sum s) = checksumString s
  checksumString (SHA512Sum s) = checksumString s

  mkChecksum = undefined



data ChecksumResult a
  = ChecksumFailed FailResult
  | Matched
  | Mismatched a
  deriving (Show)


-- | Check a 'File' against the given 'Checksum'.
-- If the 'File' does not exist or the checksum command fails,
-- 'Failed' is returned.
-- If 'Checksum' does not match,
-- 'Mismatched' is returned containing the actual 'Checksum'.
-- Otherwise 'Matched' is returned.
check :: Checkable a => File -> a -> Craft (ChecksumResult a)
check f chksum =
  exec (checksumCommand chksum) [fromAbsFile $ f^.path] >>= \case
    Failure r -> return $ ChecksumFailed r
    Success r ->
      return $
        let actual = mkChecksum $ r ^. stdout
        in if chksum == actual
              then Mismatched actual
              else Matched
