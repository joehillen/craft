module Craft.Internal.UserGroup where

import           Craft
import           Craft.Internal.Helpers

import           Control.Exception (tryJust)
import           Control.Monad (guard, void)
import           Data.List (intercalate)
import           Data.Maybe (catMaybes, fromJust)
import           System.IO.Error (isDoesNotExistError)

import           Text.Megaparsec
import           Text.Megaparsec.String

type UserName = String
type UserID = Int
type GroupID = Int

data User
  = User
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

colon = char ':'

-- TESTME
passwdParser :: Parser (UserName, UserID, GroupID, String, FilePath, FilePath)
passwdParser = do
  name <- someTill anyChar colon
  _ <- manyTill anyChar colon
  uid <- read <$> someTill digitChar colon
  gid <- read <$> someTill digitChar colon
  comment <- manyTill anyChar colon
  home <- manyTill anyChar colon
  shell <- manyTill anyChar (void eol <|> eof)
  return (name, uid, gid, comment, home, shell)

-- TESTME
shadowParser :: Parser String
shadowParser = do
  _name <- someTill anyChar colon
  manyTill anyChar colon


userFromName :: UserName -> Craft (Maybe User)
userFromName name = do
  getent "passwd" name >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> do
      let (nameS, uid', gid', comment', home', shell') =
            parseGetent passwdParser "passwd" name (stdout r)
      grp <- fromJust <$> groupFromID gid'
      grps <- getGroups nameS
      shadowResult <- errorOnFail <$> getent "shadow" nameS
      let passwordHash' = parseGetent shadowParser "shadow" nameS
                            $ stdout shadowResult
      return . Just
            $ User { username     = nameS
                    , uid          = uid'
                    , group        = grp
                    , groups       = grps
                    , passwordHash = passwordHash'
                    , home         = home'
                    , shell        = shell'
                    , comment      = comment'
                    }

getGroups :: UserName -> Craft [GroupName]
getGroups name = words . stdout . errorOnFail <$> exec "id" ["-nG", name]

getent :: String -> String -> Craft ExecResult
getent dbase key = exec "getent" [dbase, key]

parseGetent :: Parsec String a -> String -> String -> String -> a
parseGetent parser dbase key input =
  case parse parser (unwords ["getent", dbase, key]) input of
    Left  err -> error $ show err
    Right r   -> r


userFromID :: UserID -> Craft (Maybe User)
userFromID = userFromName . show


instance Craftable User where
  checker = userFromName . username

  crafter User{..} _ = do
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

  destroyer _ = notImplemented "destroyer User"

type GroupName = String

data Group
  = Group
    { groupname :: GroupName
    , gid       :: GroupID
    , members   :: [UserName]
    }
  deriving (Eq, Show)


-- TESTME
groupParser :: Parser Group
groupParser = do
  name <- someTill anyChar colon
  _ <- manyTill anyChar colon
  gid <- read <$> someTill digitChar colon
  members <- some alphaNumChar `sepBy` char ','
  return $ Group name gid members


groupFromName :: GroupName -> Craft (Maybe Group)
groupFromName gname = do
  getent "group" gname >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return
                  . Just
                  $ parseGetent groupParser "group" gname (stdout r)


groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID = groupFromName . show


instance Craftable Group where
  checker = groupFromName . groupname

  crafter g@Group{..} _ = do
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" gid ++ [groupname]
    exec_  "/usr/bin/gpasswd" ["--members", intercalate "," members, groupname]

  destroyer _ = notImplemented "destroyer Group"
