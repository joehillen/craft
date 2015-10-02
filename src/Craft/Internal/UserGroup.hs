module Craft.Internal.UserGroup
( module Craft.Internal.UserGroup
, UserID
, GroupID
)
where

import           Craft
import           Craft.Internal.Helpers

import           Control.Exception (tryJust)
import           Control.Monad (guard)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes, fromJust)
import           System.IO.Error (isDoesNotExistError)
import           System.Posix

type UserName = String

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
  r <- exec "id" ["-u", name]
  case exitcode r of
    ExitFailure _ -> return Nothing
    ExitSuccess   -> do
      grpr <- exec "id" ["-ng", name]
      grp <- fromJust <$> groupFromName (stdout grpr)
      grps <- words . stdout <$> exec "id" ["-nG", name]
      return . Just
             $ User { username     = name
                    , uid          = read $ stdout r
                    , group        = grp
                    , groups       = grps
                    , passwordHash = "a7sydna87sdn7ayd8asd"
                    , home         = "/home/" ++ name
                    , shell        = "/bin/bash"
                    , comment      = ""
                    }



-- userFromName :: UserName -> Craft (Maybe User)
-- userFromName un = do
--   eue <- tryJust (guard . isDoesNotExistError)
--                  (getUserEntryForName un)
--   case eue of
--     Left  _  -> return Nothing
--     Right ue -> Just <$> userEntryToUser ue

userFromID :: UserID -> Craft (Maybe User)
userFromID uid = do
  r <- exec "id" ["-nu", show uid]
  case exitcode r of
    ExitFailure _ -> return Nothing
    ExitSuccess   -> userFromName $ stdout r

-- userFromID ui = do
--   eue <- tryJust (guard . isDoesNotExistError)
--                  (getUserEntryForID ui)
--   case eue of
--     Left  _  -> return Nothing
--     Right ue -> Just <$> userEntryToUser ue

userEntryToUser :: UserEntry -> Craft User
userEntryToUser ue =
  groupFromID (userGroupID ue) >>= \case
    Nothing -> error $
      "User `" ++ userName ue ++ "` found, "
      ++ "but user's group does not exist!"
    Just g -> do
      let un = userName ue
      gs <- mapM groupFromName =<< memberOf un
      return
        User { username = un
             , uid = userID ue
             , group = g
             , groups = groupname <$> catMaybes gs
             , passwordHash = userPassword ue
             , home = homeDirectory ue
             , shell = userShell ue
             , comment = userGecos ue
             }


memberOf :: UserName -> Craft [GroupName]
memberOf = notImplemented "memberOf"

-- memberOf :: UserName -> Craft [GroupName]
-- memberOf un = do
--   ges <- getAllGroupEntries
--   return $ groupName <$> filter (elem un . groupMembers) ges

instance Craftable User where
  checker = userFromName . username

  crafter Root = return ()
  crafter user@User{..} = do
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

toGroupName :: GroupID -> Craft GroupName
toGroupName = notImplemented "toGroupName"

-- toGroupName :: GroupID -> Craft GroupName
-- toGroupName i = groupName <$> getGroupEntryForID i

data Group
  = RootGroup
  | Group
    { groupname :: GroupName
    , gid       :: GroupID
    , members   :: [UserName]
    }
  deriving (Eq, Show)

groupFromName :: GroupName -> Craft (Maybe Group)
groupFromName gname = notImplemented "groupFromName"

{-
groupFromName :: GroupName -> Craft (Maybe Group)
groupFromName gn = do
  ege <- tryJust (guard . isDoesNotExistError)
                      (getGroupEntryForName gn)
  case ege of
    Left  _  -> return Nothing
    Right ge -> return . Just $ groupEntryToGroup ge
-}

groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID gid' = notImplemented "groupFromID"

{-
groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID gi = do
  ege <- tryJust (guard . isDoesNotExistError)
                 (getGroupEntryForID gi)
  case ege of
    Left  _  -> return Nothing
    Right ge -> return . Just $ groupEntryToGroup ge
-}

groupEntryToGroup :: GroupEntry -> Group
groupEntryToGroup ge =
  Group { groupname = groupName ge
        , gid       = groupID ge
        , members   = groupMembers ge
        }

instance Craftable Group where
  checker = groupFromName . groupname

  crafter RootGroup = return ()
  crafter g@Group{..} = do
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" gid ++ [groupname]
    exec_  "/usr/bin/gpasswd" ["--members", intercalate "," members, groupname]

  remover _ = notImplemented "remover Group"
