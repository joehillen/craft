module Craft.File
( module Craft.File
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Formatting

import qualified Craft.Group     as Group
import           Craft.Internal
import qualified Craft.User      as User


getOwner :: File -> Craft User
getOwner f =
  User.fromID (f ^. ownerID) >>= \case
    Nothing -> $craftError $ formatToString ("No such owner with `"%shown%"` for: "%shown)
                                            (f ^. ownerID) f
    Just g  -> return g


getGroup :: File -> Craft Group
getGroup f =
  Group.fromID (f ^. groupID) >>= \case
    Nothing -> $craftError $ formatToString ("No such group with `"%shown%"` for: "%shown)
                                            (f ^. groupID) f
    Just g -> return g


name :: Lens' File RelFilePath
name = fileName


multiple :: [AbsFilePath] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode' owner' group' content' = map go paths
 where
  go path' = File path' mode' (owner'^.uid) (group'^.gid) content'


setStats :: File -> Craft ()
setStats f = do
  let fp = f^.path
  setMode    (f^.mode)    fp
  setOwnerID (f^.ownerID) fp
  setGroupID (f^.groupID) fp


write :: AbsFilePath -> ByteString -> Craft ()
write = fileWrite


exists :: AbsFilePath -> Craft Bool
exists fp = isSuccess <$> exec "test" ["-f", fromAbsFile fp]


get :: AbsFilePath -> Craft (Maybe File)
get fp = do
  r <- getStats fp
  return $ do
    (m, o, g) <- r
    return $
      file fp
      & mode    .~ m
      & ownerID .~ o
      & groupID .~ g
      & fileContent .~ Nothing


getWithContent :: AbsFilePath -> Craft (Maybe File)
getWithContent fp =
  get fp >>= \case
    Nothing -> return Nothing
    Just f  -> do
      content' <- fileRead fp
      return . Just $ f & fileContent ?~ content'


-- TODO: fallback if md5sum is not available
-- XXX: use sha256sum?
md5sum :: AbsFilePath -> Craft String
md5sum fp = head . words <$> ($stdoutOrError =<< exec "md5sum" [fromAbsFile fp])


-- | A thin wrapper over the Unix find program.
find :: AbsDirPath -> Args -> Craft [File]
find dir args = do
  fs <- mapM parseAbsFile . lines =<< $stdoutOrError =<< exec "find" ([fromAbsDir dir] ++ args ++ [ "-type", "f"])
  catMaybes <$> (mapM get fs)
