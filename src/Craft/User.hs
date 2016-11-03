module Craft.User where

import           Control.Lens             hiding (un)
import           Data.List                (intercalate)
import qualified Data.Set                 as S
import           Formatting

import qualified Craft.Group              as Group
import           Craft.Internal
import           Craft.Internal.Helpers
import           Craft.Internal.UserGroup


type Name = UserName


name :: Lens' User Name
name = userName


gid :: Lens' User GroupID
gid = userGroup . Craft.Internal.gid


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

  , _optHome       :: AbsDirPath

  , _optCreateHome :: Bool
  -- ^ Create the user's home directory when creating user

  , _optPassword   :: Maybe String

  , _optSalt       :: Maybe String
  -- ^ The salt to use when creating the user's password


  --, password_max_age :: Maybe Int
  --, password_min_age :: Maybe Int

  --, _optLocked     :: Bool
  -- Lock the user's account

  , _optShell      :: Maybe AbsFilePath
  -- User's shell

  , _optSystem     :: Bool
  -- Is the user a system user. Default: False
  }
  deriving (Eq, Show)
makeLenses ''UserOptions


-- Nothing means rely on the system's default behavior
userOptions :: String -> Craft UserOptions
userOptions n = do
  dirName <- parseRelDir n
  return UserOptions
         { _optName       = n
         , _optUID        = Nothing
         , _optComment    = n
         , _optGroup      = Nothing
         , _optUserGroup  = True
         , _optGroups     = []
         , _optHome       = $(mkAbsDir "/home")</>dirName
         , _optCreateHome = True
         , _optPassword   = Nothing
         , _optSalt       = Nothing
         , _optShell      = Nothing
         , _optSystem     = False
         --, _optLocked     = False
         }

systemUserOptions :: String -> Craft UserOptions
systemUserOptions n = do
  uo <- userOptions n
  return $ uo & optHome       .~ $(mkAbsDir "/")
              & optShell      ?~ $(mkAbsFile "/usr/sbin/nologin")
              & optCreateHome .~ False
              & optPassword   ?~ ""
              & optSystem     .~ True
              -- & optLocked     .~ True


userMod :: UserName -> [String] -> Craft ()
userMod (UserName un) args =
  exec_ "usermod" $ args ++ [un]


getUID :: UserName -> Craft (Maybe UserID)
getUID (UserName "root") = return . Just $ UserID 0
getUID (UserName un) =
  exec "id" ["--user", un] >>= \case
    Success r -> return . Just . UserID . read $ r ^. stdout
    Failure _ -> return Nothing


setUID :: UserName -> UserID -> Craft ()
setUID un uid' = userMod un ["--uid", show uid']


setShell :: UserName -> AbsFilePath -> Craft ()
setShell un shell' = userMod un ["--shell", fromAbsFile shell']


setComment :: UserName -> String -> Craft ()
setComment un comment' = userMod un ["--comment", comment']


setGroup :: UserName -> GroupName -> Craft ()
setGroup un gn =
  Group.fromName gn  >>= \case
    Just g  -> userMod un $ toArg "--gid" (g ^. Craft.Internal.gid)
    Nothing -> $craftError $ formatToString ("setGroup `"%shown%"` `"%shown%"` failed. Group `"%shown%"` not found!") un gn gn


setGroups :: UserName -> [GroupName] -> Craft ()
setGroups _  []  = return ()
setGroups un gns = userMod un ["--groups", intercalate "," $ map show gns]


setHome :: UserName -> AbsDirPath -> Craft ()
setHome un homepath = userMod un ["--home", fromAbsDir homepath]


ensureUserOpts :: User -> UserOptions -> Craft Bool
ensureUserOpts user UserOptions{..} = do
  let checks =
        [ maybeOpt _optUID     (user^.uid)                   setUID
        , opt      _optComment (user^.userComment)           setComment
        , maybeOpt _optGroup   (user^.userGroup . groupName) setGroup
        , opt      _optHome    (user^.userHome)              setHome
        , maybeOpt _optShell   (user^.userShell)             setShell
        , if sameElems _optGroups (user^.userGroups)
            then setGroups username _optGroups >> return True
            else return False
        --TODO: password, salt, lock
        ] :: [Craft Bool]
  or <$> sequence checks
   where
    username = UserName _optName
    opt :: Eq a => a -> a -> (UserName -> a -> Craft ()) -> Craft Bool
    opt expected actual setter
      | expected == actual = return False
      | otherwise          = setter username expected >> return True
    maybeOpt :: Eq a => Maybe a -> a -> (UserName -> a -> Craft ()) -> Craft Bool
    maybeOpt Nothing _ _                   = return False
    maybeOpt (Just expected) actual setter = opt expected actual setter


sameElems :: Ord a => [a] -> [a] -> Bool
sameElems xs ys = S.fromList xs == S.fromList ys


createUser :: UserOptions -> Craft ()
createUser UserOptions{..} = do
  groupArg <- getGroupArg _optGroup
  exec_ "useradd" $ optsToArgs ++ groupArg ++ groupsArg ++ [show _optName]
 where
  getGroupArg :: Maybe GroupName -> Craft [String]
  getGroupArg Nothing = return []
  getGroupArg (Just gn) =
    Group.fromName gn >>= \case
      Just g  -> return $ toArg "--gid" $ g ^. Craft.Internal.gid
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
