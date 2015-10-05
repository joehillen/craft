module Craft.Internal.UserGroup where

import           Craft
import           Craft.Internal.Helpers

import           Control.Exception (tryJust)
import           Control.Monad (guard)
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Maybe (catMaybes, fromJust)
import           System.IO.Error (isDoesNotExistError)

type UserName = String
type UserID = Int
type GroupID = Int

data User
  = Root
  | User
    { username     :: UserName
    , uid          :: UserID
    , comment      :: String
    , group        :: Group
    , groups       :: [GroupName]
    , home         :: FilePath
    , passwordHash :: String
    --, salt         :: String
    --, locked       :: Bool
    , shell        :: FilePath
    --, system       :: Bool
    }
 deriving (Eq, Show)

userFromName :: UserName -> Craft (Maybe User)
userFromName name = do
  r <- getent "passwd" name
  case exitcode r of
    ExitFailure _ -> return Nothing
    ExitSuccess   -> do
      let [_, _, uidS, gidS, comment', home', shell'] = parsePasswd $ stdout r
      grp <- fromJust <$> groupFromID (read gidS)
      grps <- getGroups name
      passwordHash' <- (!! 1) . splitOn ":" . stdout <$> getent "shadow" name
      return . Just
             $ User { username     = name
                    , uid          = read uidS
                    , group        = grp
                    , groups       = grps
                    , passwordHash = passwordHash'
                    , home         = home'
                    , shell        = shell'
                    , comment      = comment'
                    }

getGroups :: UserName -> Craft [GroupName]
getGroups name = words . stdout <$> exec "id" ["-nG", name]

parsePasswd :: String -> [String]
parsePasswd = splitOn ":"

getent :: String -> String -> Craft ExecResult
getent dbase key = exec "getent" [dbase, key]


userFromID :: UserID -> Craft (Maybe User)
userFromID = userFromName . show


memberOf :: UserName -> Craft [GroupName]
memberOf = notImplemented "memberOf"

-- memberOf :: UserName -> Craft [GroupName]
-- memberOf un = do
--   ges <- getAllGroupEntries
--   return $ groupName <$> filter (elem un . groupMembers) ges

instance Craftable User where
  checker = userFromName . username

  crafter Root = return ()
  crafter User{..} = do
    g <- groupFromName (groupname group) >>= \case
      Nothing -> craft group
      Just g  -> return g
    exec_ "/usr/sbin/useradd" $ args ++ toArg "--gid" (gid g)
   where
    args = Prelude.concat
      [ toArg "--uid"         uid
      , toArg "--comment"     comment
      , toArg "--groups"      $ intercalate "," groups
      , toArg "--home"        home
      , toArg "--password"    passwordHash
      , toArg "--shell"       shell
      ]

  remover _ = notImplemented "remover User"

type GroupName = String

data Group
  = RootGroup
  | Group
    { groupname :: GroupName
    , gid       :: GroupID
    , members   :: [UserName]
    }
  deriving (Eq, Show)


groupFromName :: GroupName -> Craft (Maybe Group)
groupFromName gname = do
  r <- getent "group" gname
  return $ case exitcode r of
    ExitFailure _ -> Nothing
    ExitSuccess   -> Just . groupFromGetent $ stdout r


groupFromGetent :: String -> Group
groupFromGetent s =
  let [name, _, gidS, membersS] = splitOn ":" s
  in Group { groupname = name
           , gid       = read gidS
           , members   = splitOn "," membersS
           }

groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID = groupFromName . show


instance Craftable Group where
  checker = groupFromName . groupname

  crafter RootGroup = return ()
  crafter g@Group{..} = do
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" gid ++ [groupname]
    exec_  "/usr/bin/gpasswd" ["--members", intercalate "," members, groupname]

  remover _ = notImplemented "remover Group"
