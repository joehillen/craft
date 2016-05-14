module Craft.SSH.AuthorizedKey where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft hiding (try)
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.Directory (Directory)
import qualified Craft.Directory as Dir
import           Craft.File.Mode
import           Craft.SSH
import           Craft.SSH.PublicKey as PK
import           Craft.User


data AuthorizedKey
  = AuthorizedKey
    { _authkeyUser :: User
    , _authkey     :: PublicKey
    }
    deriving (Show, Eq)
makeLenses ''AuthorizedKey


instance Craftable AuthorizedKey AuthorizedKey where
  watchCraft ak@(AuthorizedKey user pk) = do
    pubKeys <- Map.elems . Map.insert (pk^.pubkeyValue) pk
                         . authkeysToMap
                       <$> get user
    craft_ $ userDir user
    w <- watchCraft_ $ userFile user
           & File.strContent .~ (unlines $ map PK.toString pubKeys)
    return (w, ak)


instance Destroyable AuthorizedKey where
  watchDestroy (AuthorizedKey user pk) = do
    pubKeysMap <- authkeysToMap <$> get user
    let val = pk^.pubkeyValue
    let newPubKeys = Map.elems $ Map.delete val pubKeysMap
    craft_ $ userDir user
    watchCraft_ $ userFile user
       & File.strContent .~ (unlines $ map PK.toString newPubKeys)
    return $ case Map.lookup val pubKeysMap of
               Nothing -> (Unchanged, Nothing)
               Just x  -> (Removed, Just (AuthorizedKey user x))


userFile :: User -> File
userFile user =
  file ((user^.to userDir.Dir.path)</>"authorized_keys")
  & File.mode          .~ Mode RW O O
  & File.ownerAndGroup .~ user


get :: User -> Craft [AuthorizedKey]
get user = do
  pks <- parseFile parsePublicKeys (user ^. to userFile . File.path)
  return $ map (AuthorizedKey user) pks


authkeysToMap :: [AuthorizedKey] -> Map.Map String PublicKey
authkeysToMap = Map.fromList . map (((,)<$>_pubkeyValue<*>id) . _authkey)


parsePublicKeys :: Parser [PublicKey]
parsePublicKeys = parsePublicKey `sepEndBy` eol
