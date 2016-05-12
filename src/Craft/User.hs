
module Craft.User
( module Craft.User
, User(..)
, UserID(..)
, UserName(..)
, username
, uid
, comment
, group
, groups
, home
, passwordHash
, shell
)
where

import           Control.Lens hiding (un)
import           Control.Monad (foldM)
import           Data.List (intercalate)
import qualified Data.Set as S
import           Formatting
import           System.FilePath ((</>))

import qualified Craft.Group as Group
import           Craft.Internal
import           Craft.Internal.Helpers
import qualified Craft.Internal.UserGroup as UG
import           Craft.Internal.UserGroup hiding (gid)


type Name = UserName


name :: Lens' User Name
name = username


gid :: Lens' User GroupID
gid = group . UG.gid


data UserOptions =
  UserOptions
  { _optName       :: String
  , _optUID        :: Maybe UserID
  , _optComment    :: String

  --, optAllowdupe  :: Bool
  -- Whether to allow duplicate UIDs. Default: False

  , _optGroup      :: Maybe GroupName
  -- ^ The default group for a user

  , _optUserGroup  :: Bool
  -- ^ Create a user group for this user. Default: True

  , _optGroups     :: [GroupName]
  -- ^ Other groups

  , _optHome       :: FilePath

  , _optCreateHome :: Bool
  -- ^ Create the user's home directory when creating user

  , _optPassword   :: Maybe String

  , _optSalt       :: Maybe String
  -- ^ The salt to use when creating the user's password


  --, password_max_age :: Maybe Int
  --, password_min_age :: Maybe Int

  --, _optLocked     :: Bool
  -- Lock the user's account

  , _optShell      :: Maybe FilePath
  -- User's shell

  , _optSystem     :: Bool
  -- Is the user a system user. Default: False
  }
  deriving (Eq, Show)
makeLenses ''UserOptions


-- Nothing means rely on the system's default behavior
userOptions :: String -> UserOptions
userOptions name =
  UserOptions
  { _optName       = name
  , _optUID        = Nothing
  , _optComment    = name
  , _optGroup      = Nothing
  , _optUserGroup  = True
  , _optGroups     = []
  , _optHome       = "/home"</>name
  , _optCreateHome = True
  , _optPassword   = Nothing
  , _optSalt       = Nothing
  , _optShell      = Nothing
  , _optSystem     = False
  --, _optLocked     = False
  }

systemUserOptions :: String -> UserOptions
systemUserOptions name =
  userOptions name
  & optHome       .~ "/"
  & optShell      ?~ "/usr/sbin/nologin"
  & optCreateHome .~ False
  & optPassword   ?~ ""
  & optSystem     .~ True
  -- & optLocked     .~ True


userMod :: UserName -> [String] -> Craft ()
userMod (UserName un) args =
  exec_ "/usr/sbin/usermod" $ args ++ [un]


getUID :: UserName -> Craft (Maybe UserID)
getUID (UserName "root") = return . Just $ UserID 0
getUID (UserName un) =
  exec "/usr/bin/id" ["--user", un] >>= \case
    ExecSucc r -> return . Just . UserID . read $ r ^. stdout
    ExecFail _ -> return Nothing


setUID :: UserName -> UserID -> Craft ()
setUID un uid' = userMod un ["--uid", show uid']


setShell :: UserName -> FilePath -> Craft ()
setShell un shell' = userMod un ["--shell", shell']


setComment :: UserName -> String -> Craft ()
setComment un comment' = userMod un ["--comment", comment']


setGroup :: UserName -> GroupName -> Craft ()
setGroup un gn =
  Group.fromName gn  >>= \case
    Just g  -> userMod un $ toArg "--gid" (g ^. UG.gid)
    Nothing -> $craftError $ formatToString ("setGroup `"%shown%"` `"%shown%"` failed. Group `"%shown%"` not found!") un gn gn


setGroups :: UserName -> [GroupName] -> Craft ()
setGroups _  []  = return ()
setGroups un gns = userMod un ["--groups", intercalate "," $ map show gns]


setHome :: UserName -> FilePath -> Craft ()
setHome un path = userMod un ["--home", path]


instance Craftable UserOptions User where
  watchCraft uopts = do
    let notfound = "craft `"++uopts ^. optName++"` failed. Not Found!"
    let un = UserName $ uopts ^. optName
    fromName un >>= \case
      Nothing           -> do
        createUser uopts
        fromName un >>= \case
          Nothing          -> $craftError notfound
          Just createdUser -> do
            madeChanges <- ensureUserOpts createdUser uopts
            if not madeChanges
              then return (Created, createdUser)
              else fromName un >>= \case
                Nothing -> $craftError notfound
                Just u  -> return (Created, u)
      Just existingUser -> do
        madeChanges <- ensureUserOpts existingUser uopts
        if not madeChanges
          then return (Unchanged, existingUser)
          else fromName un >>= \case
            Nothing -> $craftError notfound
            Just u  -> return (Updated, u)


ensureUserOpts :: User -> UserOptions -> Craft Bool
ensureUserOpts user uopts@UserOptions{..} = do
  let checks =
        [ maybeOpt _optUID     (user ^. uid)               setUID
        , opt      _optComment (user ^. comment)           setComment
        , maybeOpt _optGroup   (user ^. group . groupname) setGroup
        , opt      _optHome    (user ^. home)              setHome
        , maybeOpt _optShell   (user ^. shell)             setShell
        , if sameElems _optGroups (user ^. groups)
            then setGroups un _optGroups >> return True
            else return False
        --TODO: password, salt, lock
        ] :: [Craft Bool]
  or <$> sequence checks
   where
    un = UserName _optName
    opt expected actual setter
      | expected == actual = return False
      | otherwise          = setter un expected >> return True
    maybeOpt Nothing _ _                   = return False
    maybeOpt (Just expected) actual setter = opt expected actual setter


sameElems :: Ord a => [a] -> [a] -> Bool
sameElems xs ys = S.fromList xs == S.fromList ys


createUser :: UserOptions -> Craft ()
createUser UserOptions{..} = do
  groupArg <- getGroupArg _optGroup
  exec_ "/usr/sbin/useradd" $ optsToArgs ++ groupArg ++ groupsArg ++ [show _optName]
 where
  getGroupArg :: Maybe GroupName -> Craft [String]
  getGroupArg Nothing = return []
  getGroupArg (Just gn) =
    Group.fromName gn >>= \case
      Just g  -> return $ toArg "--gid" $ g ^. Group.gid
      Nothing -> $craftError $ "Failed to create User `"++_optName++"` " ++ "with group `"++show gn++"`. Group not found!"
  groupsArg = toArg "--groups" $ intercalate "," (map show _optGroups)
  optsToArgs =
    concat
      [ toArg "--uid"         _optUID
      , toArg "--comment"     _optComment
      , toArg "--home"        _optHome
      , toArg "--create-home" _optCreateHome
      , toArgBool "--user-group" "--no-user-group" _optUserGroup
      , toArg "--password"    (encrypt _optSalt _optPassword)
      , toArg "--shell"       _optShell
      , toArg "--system"      _optSystem
      ]


lock :: UserName -> Craft ()
lock un = userMod un ["--lock"]


fromName :: Name -> Craft (Maybe User)
fromName (UserName n) = userFromStr n


fromID :: UserID -> Craft (Maybe User)
fromID = userFromID


--   ___ ___ _____   ___ _____ ___
--  | _ \ _ \_ _\ \ / /_\_   _| __|
--  |  _/   /| | \ V / _ \| | | _|
--  |_| |_|_\___| \_/_/ \_\_| |___|


-- TODO
encrypt :: Maybe String -> Maybe String -> Maybe String
encrypt _ _ = Nothing
