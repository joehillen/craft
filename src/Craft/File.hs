{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import           System.FilePath

import           Craft.Internal
import qualified Craft.User as User
import qualified Craft.Group as Group


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


name :: Getter File String
name = filePath . to takeFileName


fromSource :: FilePath -> FilePath -> Craft File
fromSource sourcefp fp = do
  c <- readSourceFile sourcefp
  return $ file fp & fileContent ?~ c


multiple :: [FilePath] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode' owner' group' content' = map go paths
 where
  go path' = File path' mode' (owner' ^. uid) (group' ^. gid) content'


setStats :: File -> Craft ()
setStats f = do
  let fp = f ^. path
  setMode    (f ^. mode)    fp
  setOwnerID (f ^. ownerID) fp
  setGroupID (f ^. groupID) fp


write :: FilePath -> ByteString -> Craft ()
write = fileWrite


exists :: FilePath -> Craft Bool
exists fp = isExecSucc <$> exec "test" ["-f", fp]


get :: FilePath -> Craft (Maybe File)
get fp =
  getStats fp >>= \case
    Nothing -> return Nothing
    Just (m, o, g) -> do
      return . Just $ file fp & mode    .~ m
                              & ownerID .~ o
                              & groupID .~ g
                              & fileContent .~ Nothing


getWithContent :: FilePath -> Craft (Maybe File)
getWithContent fp =
  get fp >>= \case
    Nothing -> return Nothing
    Just f  -> do
      content' <- fileRead fp
      return . Just $ f & fileContent ?~ content'


md5sum :: FilePath -> Craft String
md5sum "" = $craftError "md5sum on empty file path"
md5sum fp = head . words <$> ($stdoutOrError =<< exec "md5sum" [fp])


-- | A thin wrapper over the Unix find program.
find :: FilePath -> Args -> Craft [File]
find dir args = do
  s <- $stdoutOrError =<< exec "find" ([dir, "-type", "f"] ++ args)
  catMaybes <$> (mapM get . filter (`notElem` [".", "..", ""]) $ lines s)
