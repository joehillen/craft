module Craft.File
( module Craft.File
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import           Craft.Helpers
import           Craft.Internal
import           Craft.File.Mode
import           Craft.User (User, UserID(..))
import qualified Craft.User as User
import           Craft.Group (Group, GroupID(..))
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory

import Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lens
import           Data.Maybe
import Formatting
import           System.FilePath


data File
  = File
    { _path    :: FilePath
    , _mode    :: Mode
    , _ownerID :: UserID
    , _groupID :: GroupID
    , _content :: Maybe ByteString
    }

makeLenses ''File


instance Eq File where
  (==) a b = (a ^. path == b ^. path)
          && (a ^. mode == b ^. mode)
          && (a ^. ownerID == b ^. ownerID)
          && (a ^. groupID == b ^. groupID)
          && (  isNothing (a ^. content)
             || isNothing (b ^. content)
             || (a ^. content == b ^. content))


owner :: Setter File File () User
owner = sets (\functor f -> doit f (functor ()))
 where doit f o = f & ownerID .~ (o ^. User.uid)


group :: Setter File File () Group
group = sets (\functor f -> doit f (functor ()))
 where doit f g = f & groupID .~ (g ^. Group.gid)


ownerAndGroup :: Setter File File () User
ownerAndGroup = sets (\functor f -> doit f (functor ()))
 where doit f u = f & owner .~ u
                    & group .~ (u ^. User.group)


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


instance Show File where
  show f = "File { _path = " ++ show (f ^. path) ++
                ", _mode = " ++ show (f ^. mode) ++
                ", _ownerID = " ++ show (f ^. ownerID) ++
                ", _groupID = " ++ show (f ^. groupID) ++
                ", _content = " ++ showContent (f ^. content) ++
               " }"
    where
      showContent Nothing  = "Nothing"
      showContent (Just c) = "Just " ++ show (BS.take 30 c) ++ "..."


name :: Getter File String
name = path . to takeFileName


strContent :: Lens' File String
strContent = lens (view $ content . _Just . unpackedChars)
                  (\f s -> f & content .~ Just (B8.pack s))



file :: FilePath -> File
file fp =
  File
  { _path    = fp
  , _mode    = Mode RW R R
  , _ownerID = UserID 0
  , _groupID = GroupID 0
  , _content = Nothing
  }


fromSource :: FilePath -> FilePath -> Craft File
fromSource sourcefp fp = do
  c <- readSourceFile sourcefp
  return $ file fp & content ?~ c


multiple :: [FilePath] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode' owner' group' content' = map go paths
 where
  go path' = File path' mode' (owner' ^. User.uid) (group' ^. Group.gid) content'



instance Craftable File File where
  watchCraft f = do
    let fp = f ^. path
    let expectedMD5 = show . md5 . BL.fromStrict $ f ^. content . _Just
    let err str = $craftError $ "craft File `" ++ fp ++ "` failed! " ++ str
    let verifyMode m =
          when (m /= f ^. mode) $
            err $ "Wrong Mode: " ++ show m ++ " Expected: " ++ show (f ^. mode)
    let verifyOwner o =
          when (o /= f ^. ownerID) $
            err $ "Wrong OwnerID: " ++ show o ++ " Expected: " ++ show (f ^. ownerID)
    let verifyGroup g =
          when (g /= f ^. groupID) $
            err $ "Wrong GroupID: " ++ show g ++ " Expected: " ++ show (f ^. groupID)
    let verifyStats (m, o, g) = verifyMode m >> verifyOwner o >> verifyGroup g
    getStats fp >>= \case
      Nothing          -> do
        case f ^. content of
          Nothing -> exec_ "touch" [fp]
          Just c  -> write fp c
        setStats f
        when (isJust $ f  ^. content) $ do
          actualMD5 <- md5sum fp
          unless (actualMD5 == expectedMD5) $
            err "Content Mismatch."
        return (Created, f)
      Just (m', o', g') -> do
        let checks = [ (f ^. mode    == m', setMode    (f ^. mode)    fp)
                     , (f ^. ownerID == o', setOwnerID (f ^. ownerID) fp)
                     , (f ^. groupID == g', setGroupID (f ^. groupID) fp)
                     ]
        mapM_ (uncurry unless) checks
        case f ^. content of
          Nothing -> if all fst checks
                     then return (Unchanged, f)
                     else do
                       getStats fp >>= \case
                         Nothing    -> err "Not Found."
                         Just stats -> verifyStats stats
                       return (Updated, f)
          Just c -> do
            actualMD5 <- md5sum fp
            if actualMD5 == expectedMD5
            then return $ if all fst checks
                          then (Unchanged, f)
                          else (Updated, f)
            else do
              write fp c
              md5AfterWrite <- md5sum fp
              if md5AfterWrite == expectedMD5
                then return (Updated, f)
                else err "Content Mismatch."

  craft f = do
    let fp = f ^. path
    exec_ "touch" ["-a", fp]
    case f ^. content of
      Nothing -> return ()
      Just c  -> do
        let expectedMD5 = show . md5 $ BL.fromStrict c
        actualMD5 <- md5sum fp
        unless (actualMD5 == expectedMD5) $ do
          write fp c
          md5AfterWrite <- md5sum fp
          unless (expectedMD5 == md5AfterWrite) $
            $craftError $ "craft File `" ++ fp ++ "` failed! Content Mismatch."
    setStats f
    return f

setStats :: File -> Craft ()
setStats f = do
  let fp = f ^. path
  setMode    (f ^. mode)    fp
  setOwnerID (f ^. ownerID) fp
  setGroupID (f ^. groupID) fp


instance Destroyable File where
  watchDestroy f =
    get (f ^. path) >>= \case
      Nothing -> return (Unchanged, Nothing)
      Just f' -> do
        destroy_ f
        return (Removed, Just f')

  destroy_ f = do
    let fp = f ^. path
    exec_ "rm" ["-f", fp]
    exists fp >>= flip when (
      $craftError $ "destroy File `" ++ fp ++ "` failed! Found.")


write :: FilePath -> ByteString -> Craft ()
write = fileWrite


exists :: FilePath -> Craft Bool
exists fp = isExecSucc <$> exec "/usr/bin/test" ["-f", fp]


get :: FilePath -> Craft (Maybe File)
get fp =
  getStats fp >>= \case
    Nothing -> return Nothing
    Just (m, o, g) -> do
      return . Just $ file fp & mode    .~ m
                              & ownerID .~ o
                              & groupID .~ g
                              & content .~ Nothing


getWithContent :: FilePath -> Craft (Maybe File)
getWithContent fp =
  get fp >>= \case
    Nothing -> return Nothing
    Just f  -> do
      content' <- fileRead fp
      return . Just $ f & content ?~ content'


md5sum :: FilePath -> Craft String
md5sum "" = $craftError "md5sum on empty file path"
md5sum fp = head . words <$> ($stdoutOrError =<< exec "md5sum" [fp])


-- | A thin wrapper over the Unix find program.
find :: FilePath -> Args -> Craft [File]
find dir args = do
  s <- $stdoutOrError =<< exec "find" ([dir, "-type", "f"] ++ args)
  catMaybes <$> (mapM get . filter (`notElem` [".", "..", ""]) $ lines s)
