module Craft.SSH.AuthorizedKey where

import           Control.Lens
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Text.Megaparsec
import           Text.Megaparsec.String

import           Craft hiding (try)
import qualified Craft.File as File
import           Craft.SSH
import           Craft.SSH.PublicKey as PK


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
                       & strContent .~ (unlines $ map PK.toString pubKeys)
    return (w, ak)


instance Destroyable AuthorizedKey where
  watchDestroy (AuthorizedKey user pk) = do
    pubKeysMap <- authkeysToMap <$> get user
    let val = pk^.pubkeyValue
    let newPubKeys = Map.elems $ Map.delete val pubKeysMap
    craft_ $ userDir user
    craft_ $ userFile user
             & strContent .~ (unlines $ map PK.toString newPubKeys)
    return $ case Map.lookup val pubKeysMap of
               Nothing -> (Unchanged, Nothing)
               Just x  -> (Removed, Just (AuthorizedKey user x))


userFile :: User -> File
userFile user =
  file ((user^.to userDir.path)</>"authorized_keys")
  & mode          .~ Mode RW O O
  & ownerAndGroup .~ user


get :: User -> Craft [AuthorizedKey]
get user = File.get (user ^. to userFile . path) >>= \case
  Nothing -> return []
  Just f  -> map (AuthorizedKey user) <$> parseFile parsePublicKeys (f^.path)


authkeysToMap :: [AuthorizedKey] -> Map.Map String PublicKey
authkeysToMap = Map.fromList . map (((,)<$>_pubkeyValue<*>id) . _authkey)


parsePublicKeys :: Parser [PublicKey]
parsePublicKeys = do
  keys <- (try (Just <$> parsePublicKey) <|> (space >> return Nothing)) `sepEndBy` eol
  return $ catMaybes keys
