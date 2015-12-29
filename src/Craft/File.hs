module Craft.File
( module Craft.File
, setGroupID
, setOwnerID
, getOwnerID
, getGroupID
, getMode
)
where

import Control.Lens
import           Craft.Helpers
import           Craft.Internal
import           Craft.File.Mode
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (Group, GroupID)
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


owner :: File -> Craft User
owner f =
  User.fromID (f ^. ownerID) >>= \case
    Nothing -> $craftError $ formatToString ("No such owner with id `"%int%"` for: "%shown)
                                            (f ^. ownerID) f
    Just g  -> return g


group :: File -> Craft Group
group f =
  Group.fromID (f ^. groupID) >>= \case
    Nothing -> $craftError $ formatToString ("No such group with id `"%int%"` for: "%shown)
                                            (f ^. groupID) f
    Just g -> return g


instance Show File where
  show f = "File { path = " ++ show (f ^. path) ++
                ", mode = " ++ show (f ^. mode) ++
                ", ownerID = " ++ show (f ^. ownerID) ++
                ", groupID = " ++ show (f ^. groupID) ++
                ", content = " ++ showContent (f ^. content) ++
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
  , _ownerID = 0
  , _groupID = 0
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



instance Craftable File where
  watchCraft f = do
    let fp = f ^. path
        setMode'  = setMode (f ^. mode) fp
        setOwner' = setOwnerID (f ^. ownerID) fp
        setGroup' = setGroupID (f ^. groupID) fp
        md5c = show . md5 . BL.fromStrict . fromMaybe "" $ f ^. content
        err str = $craftError $ "craft File `" ++ fp ++ "` failed! " ++ str
        verifyMode m =
          when (m /= f ^. mode) $ err $ "Wrong Mode: " ++ show m
                                    ++ " Expected: " ++ show (f ^. mode)
        verifyOwner o =
          when (o /= f ^. ownerID) $ err $ "Wrong Owner ID: " ++ show o
                                    ++ " Expected: " ++ show (f ^. ownerID)
        verifyGroup g =
          when (g /= f ^. groupID) $ err $ "Wrong Group ID: " ++ show g
                                    ++ " Expected: " ++ show (f ^. groupID)
        verifyStats (m, o, g) =
          verifyMode m >> verifyOwner o >> verifyGroup g

    getStats fp >>= \case
      Nothing -> do
        exec_ "touch" [fp]
        setMode' >> setOwner' >> setGroup'
        case f ^. content of
          Nothing -> return ()
          Just c -> write fp c
        getStats fp >>= \case
          Nothing -> err "Not Found."
          Just stats' -> do
            verifyStats stats'
            case f  ^. content of
              Nothing -> return (Created, f)
              Just _ -> do
                md5content' <- md5sum fp
                if md5content' == md5c then return (Created, f)
                                       else err "Content Mismatch."

      Just (m', o', g') -> do
        let checks = [ (f ^. mode == m', setMode')
                     , (f ^. ownerID == o', setOwner')
                     , (f ^. groupID == g', setGroup')
                     ]
        mapM_ (uncurry unless) checks
        case f ^. content of
          Nothing ->
            if all fst checks then
              return (Unchanged, f)
            else do
              getStats fp >>= \case
                Nothing -> err "Not Found."
                Just stats'' -> verifyStats stats''
              return (Updated, f)

          Just c -> do
            md5content' <- md5sum fp
            if md5content' == md5c then
              if all fst checks then return (Unchanged, f)
                                      else return (Updated, f)
            else do
              write fp c
              md5content'' <- md5sum fp
              if md5content'' == md5c then return (Updated, f)
                                      else err "Content Mismatch."


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
      content' <- fileRead fp
      return . Just $ file fp & mode    .~ m
                              & ownerID .~ o
                              & groupID .~ g
                              & content ?~ content'


md5sum :: FilePath -> Craft String
md5sum fp = do
  r <- $errorOnFail =<< exec "md5sum" [fp]
  return $ r ^. stdout . to words . _head


-- | A thin wrapper over the Unix find program.
find :: FilePath -> Args -> Craft [File]
find dir args = do
  r <- $errorOnFail =<< exec "find" ([dir, "-type", "f"] ++ args)
  let filenames = filter (`notElem` [".", "..", ""]) $ r ^. stdout . to lines
  catMaybes <$> mapM get filenames
