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
import           Craft.User (User, UserID)
import qualified Craft.User as User
import           Craft.Group (Group, GroupID)
import qualified Craft.Group as Group
import           Craft.Internal.FileDirectory

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import           System.FilePath


data File
  = File
    { path    :: FilePath
    , mode    :: Mode
    , ownerID :: UserID
    , groupID :: GroupID
    , content :: Maybe ByteString
    }


instance Eq File where
  (==) a b = (path a == path b)
          && (mode a == mode b)
          && (ownerID a == ownerID b)
          && (groupID a == groupID b)
          && (  isNothing (content a)
             || isNothing (content b)
             || (content a == content b))


owner :: File -> Craft User
owner f =
  User.fromID (ownerID f) >>= \case
    Nothing -> $craftError $ "No such owner with id `" ++ show (ownerID f)
                             ++ "` for: " ++ show f
    Just g  -> return g


group :: File -> Craft Group
group f =
  Group.fromID (groupID f) >>= \case
    Nothing -> $craftError $ "No such group with id `" ++ show (groupID f)
                             ++ "` for: " ++ show f
    Just g -> return g


instance Show File where
  show f = "File { path = " ++ show (path f) ++
                ", mode = " ++ show (mode f) ++
                ", ownerID = " ++ show (ownerID f) ++
                ", groupID = " ++ show (groupID f) ++
                ", content = " ++ showContent (content f) ++
               " }"
    where
      showContent Nothing  = "Nothing"
      showContent (Just c) = "Just " ++ show (BS.take 30 c) ++ "..."


name :: File -> String
name f = takeFileName $ path f


strContent :: String -> Maybe ByteString
strContent = Just . B8.pack


contentAsString :: File -> String
contentAsString = B8.unpack . fromJust . content


file :: FilePath -> File
file fp =
  File
  { path    = fp
  , mode    = Mode RW R R
  , ownerID = 0
  , groupID = 0
  , content = Nothing
  }


fromSource :: FilePath -> FilePath -> Craft File
fromSource sourcefp fp = do
  c <- readSourceFile sourcefp
  return $ (file fp) { content = Just c }


multiple :: [FilePath] -> Mode -> User -> Group -> Maybe ByteString -> [File]
multiple paths mode owner' group' content = map go paths
 where
  go path = File path mode (User.uid owner') (Group.gid group') content


multipleRootOwned :: [FilePath] -> Mode -> Maybe ByteString -> [File]
multipleRootOwned paths mode content = map go paths
 where
  go path = (file path) { mode = mode
                        , content = content
                        }


instance Craftable File where
  watchCraft f = do
    let fp = path f
        setMode'  = setMode (mode f) fp
        setOwner' = setOwnerID (ownerID f) fp
        setGroup' = setGroupID (groupID f) fp
        md5c = show . md5 . BL.fromStrict . fromMaybe "" $ content f
        err str = $craftError $ "craft File `" ++ fp ++ "` failed! " ++ str
        verifyMode m =
          when (m /= mode f) $ err $ "Wrong Mode: " ++ show m
                                    ++ " Expected: " ++ show (mode f)
        verifyOwner o =
          when (o /= ownerID f) $ err $ "Wrong Owner ID: " ++ show o
                                    ++ " Expected: " ++ show (ownerID f)
        verifyGroup g =
          when (g /= groupID f) $ err $ "Wrong Group ID: " ++ show g
                                    ++ " Expected: " ++ show (groupID f)
        verifyStats (m, o, g) =
          verifyMode m >> verifyOwner o >> verifyGroup g

    getStats fp >>= \case
      Nothing -> do
        exec_ "touch" [fp]
        setMode' >> setOwner' >> setGroup'
        case content f of
          Nothing -> return ()
          Just c -> write fp c
        getStats fp >>= \case
          Nothing -> err "Not Found."
          Just stats' -> do
            verifyStats stats'
            case content f of
              Nothing -> return (Created, f)
              Just _ -> do
                md5content' <- md5sum fp
                if md5content' == md5c then return (Created, f)
                                       else err "Content Mismatch."

      Just (m', o', g') -> do
        let checks = [ (mode f == m', setMode')
                     , (ownerID f == o', setOwner')
                     , (groupID f == g', setGroup')
                     ]
        mapM_ (uncurry unless) checks
        case content f of
          Nothing ->
            if and (map fst checks) then
              return (Unchanged, f)
            else do
              getStats fp >>= \case
                Nothing -> err "Not Found."
                Just stats'' -> verifyStats stats''
              return (Updated, f)

          Just c -> do
            md5content' <- md5sum fp
            if md5content' == md5c then
              if and (map fst checks) then return (Unchanged, f)
                                      else return (Updated, f)
            else do
              write fp c
              md5content'' <- md5sum fp
              if md5content'' == md5c then return (Updated, f)
                                      else err "Content Mismatch."


instance Destroyable File where
  watchDestroy f = do
    let fp = path f
    get fp >>= \case
      Nothing -> return (Unchanged, Nothing)
      Just f' -> do
        destroy_ f
        return (Removed, Just f')

  destroy_ f = do
    let fp = path f
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
      content <- fileRead fp
      return . Just $
        File { path    = fp
             , mode    = m
             , ownerID = o
             , groupID = g
             , content = Just content
             }


md5sum :: FilePath -> Craft String
md5sum fp = head . words . stdout . errorOnFail <$> exec "md5sum" [fp]


-- | A thin wrapper over the Unix find program.
find :: FilePath -> Args -> Craft [File]
find dir args = do
  filenames <- filter (`notElem` [".", "..", ""])
               . lines . stdout . errorOnFail
               <$> exec "find" ([dir, "-type", "f"] ++ args)
  catMaybes <$> mapM get filenames
