
module Craft.User
( module Craft.User
, User(..)
, UserID
)
where

import           Control.Monad (when, unless)
import qualified Data.Set as S
import           Data.List (intercalate)
import           System.Exit

import           Craft
import           Craft.Internal.Helpers
import qualified Craft.Group as Group
import           Craft.Internal.UserGroup

type Name = UserName

name :: User -> Name
name = username

root :: User
root = Root

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
userMod un args =
  exec_ "/usr/sbin/usermod" $ args ++ [un]

getUID :: UserName -> Craft (Maybe UserID)
getUID "root" = return . Just $ 0
getUID un = do
  r <- exec "/usr/bin/id" ["--user", un]
  return $ case exitcode r of
    ExitSuccess    -> Just . read $ stdout r
    ExitFailure _  -> Nothing

setUID :: UserName -> UserID -> Craft ()
setUID un uid = userMod un ["--uid", show uid]

setShell :: UserName -> FilePath -> Craft ()
setShell un shell = userMod un ["--shell", shell]

setComment :: UserName -> String -> Craft ()
setComment un comment = userMod un ["--comment", comment]

setGroup :: UserName -> GroupName -> Craft ()
setGroup un gn =
  Group.fromName gn  >>= \case
    Nothing -> error $ "setGroup `" ++ un ++ "` `" ++ gn
                       ++ "` failed. Group `" ++ gn ++ "` not found!"
    Just g  -> userMod un ["--gid", show $ gid g]

setGroups :: UserName -> [GroupName] -> Craft ()
setGroups un gns = userMod un ["--groups", intercalate "," gns]

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
    Nothing   -> notfound
    Just Root -> return ()
    Just user -> do
      handleOpt optUID (uid user) $
        setUID un

      when (optComment /= comment user) $
        setComment un optComment

      handleOpt optGroup (groupname . group $ user) $
        setGroup un

      handleOpt optHome (home user) $
        setHome un

      unless (sameElems optGroups $ groups user) $
        setGroups un optGroups

      handleOpt optShell (shell user) $
        setShell un

      --TODO: setPassword, setSalt

      when optLocked $
        lock un

  fromName un >>= \case
    Nothing -> notfound
    Just r  -> return r
 where
  notfound = error $ "createUser `" ++ un ++ "` failed. Not Found!"

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
       optsToArgs ++ groupArg ++ groupsArg ++ [un]
 where
  getGroupArg :: Maybe GroupName -> Craft [String]
  getGroupArg Nothing = return []
  getGroupArg (Just gn) =
    Group.fromName gn >>= \case
      Nothing -> error $
        "Failed to create User `" ++ un ++ "` "
        ++ "with group `" ++ gn ++ "`. Group not found!"
      Just g -> return $ toArg "--gid" $ Group.gid g
  groupsArg = toArg "--groups" $ intercalate "," optGroups
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
fromName = userFromName

fromID :: UserID -> Craft (Maybe User)
fromID = userFromID


--   ___ ___ _____   ___ _____ ___
--  | _ \ _ \_ _\ \ / /_\_   _| __|
--  |  _/   /| | \ V / _ \| | | _|
--  |_| |_|_\___| \_/_/ \_\_| |___|


encrypt :: Maybe String -> Maybe String -> Maybe String
encrypt msalt mpassword = Nothing
