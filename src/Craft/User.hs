
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
import           Data.List (intercalate)
import qualified Data.Set as S
import           Formatting

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


data Options =
  Options
  { optUID        :: Maybe UserID
  , optComment    :: String

  --, optAllowdupe  :: Bool
  -- Whether to allow duplicate UIDs. Default: False

  , optGroup      :: Maybe GroupName
  -- The default group for a user

  , optUserGroup  :: Bool
  -- Create a user group for this user. Default: True

  , optGroups     :: [GroupName]

  , optHome       :: Maybe FilePath

  , optCreateHome :: Bool
  -- Create the user's home directory when creating user. Default: True

  , optPassword   :: Maybe String

  , optSalt       :: Maybe String
  -- The salt to use when creating the user's password


  --, password_max_age :: Maybe Int
  --, password_min_age :: Maybe Int

  , optLocked     :: Bool
  -- Lock the user's account

  , optShell      :: Maybe FilePath
  -- User's shell

  , optSystem     :: Bool
  -- Is the user a system user. Default: False
  }

-- Nothing means rely on the system's default behaviour
opts :: Options
opts =
  Options
  { optUID     = Nothing
  , optComment    = ""
  , optGroup      = Nothing
  , optUserGroup  = True
  , optGroups     = []
  , optHome       = Nothing
  , optCreateHome = True
  , optPassword   = Nothing
  , optSalt       = Nothing
  , optLocked     = False
  , optShell      = Nothing
  , optSystem     = False
  }


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
    Nothing ->
      $craftError
      $ formatToString ("setGroup `"%shown%"` `"%shown%"` failed. Group `"%shown%"` not found!")
                       un gn gn
    Just g  -> userMod un $ toArg "--gid" (g ^. UG.gid)

setGroups :: UserName -> [GroupName] -> Craft ()
setGroups _  []  = return ()
setGroups un gns = userMod un ["--groups", intercalate "," $ map show gns]

setHome :: UserName -> FilePath -> Craft ()
setHome un path = userMod un ["--home", path]

createUser :: UserName -> Options -> Craft User
createUser un uopts@Options{..} = do
  user' <- fromName un >>= \case
    Nothing -> do
      createUser' un uopts
      fromName un
    x       -> return x

  case user' of
    Nothing   -> $craftError notfound
    Just user -> do
      handleOpt optUID (user ^. uid) $
        setUID un

      when (optComment /= user ^. comment) $
        setComment un optComment

      handleOpt optGroup (user ^. group . groupname) $
        setGroup un

      handleOpt optHome (user ^. home) $
        setHome un

      unless (sameElems optGroups $ user ^. groups) $
        setGroups un optGroups

      handleOpt optShell (user ^. shell) $
        setShell un

      --TODO: setPassword, setSalt

      when optLocked $
        lock un

  fromName un >>= \case
    Nothing -> $craftError notfound
    Just r  -> return r
 where
  notfound = "createUser `" ++ show un ++ "` failed. Not Found!"

  handleOpt :: Eq a => Maybe a -> a -> (a -> Craft ()) -> Craft ()
  handleOpt Nothing       _      _ = return ()
  handleOpt (Just newval) oldval f
    | newval /= oldval = f newval
    | otherwise = return ()

sameElems :: Ord a => [a] -> [a] -> Bool
sameElems xs ys = S.fromList xs == S.fromList ys

createUser' :: UserName -> Options -> Craft ()
createUser' un Options{..} = do
  groupArg <- getGroupArg optGroup
  exec_ "/usr/sbin/useradd" $
       optsToArgs ++ groupArg ++ groupsArg ++ [show un]
 where
  getGroupArg :: Maybe GroupName -> Craft [String]
  getGroupArg Nothing = return []
  getGroupArg (Just gn) =
    Group.fromName gn >>= \case
      Nothing -> $craftError $
        "Failed to create User `" ++ show un ++ "` "
        ++ "with group `" ++ show gn ++ "`. Group not found!"
      Just g -> return $ toArg "--gid" $ g ^. Group.gid
  groupsArg = toArg "--groups" $ intercalate "," (map show optGroups)
  optsToArgs =
    concat
      [ toArg "--uid"         optUID
      , toArg "--comment"     optComment
      , toArg "--home"        optHome
      , toArg "--create-home" optCreateHome
      , toArgBool "--user-group" "--no-user-group" optUserGroup
      , toArg "--password"    (encrypt optSalt optPassword)
      , toArg "--shell"       optShell
      , toArg "--system"      optSystem
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
