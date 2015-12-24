module Craft.Internal.UserGroup where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.String

import Craft.Internal
import Craft.Internal.Helpers


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

colon :: Parser Char
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
userFromName name =
  getent "passwd" name >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> do
      let (nameS, uid', gid', comment', home', shell') =
            parseGetent passwdParser "passwd" name (r ^. stdout)
      grp <- fromJust <$> groupFromID gid'
      grps <- getGroups nameS
      shadowResult <- view errorOnFail <$> getent "shadow" nameS
      let passwordHash' = parseGetent shadowParser "shadow" nameS
                            $ shadowResult ^. stdout
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
getGroups name =  view (errorOnFail . stdout . to words) <$> exec "id" ["-nG", name]

getent :: String -> String -> Craft ExecResult
getent dbase key = exec "getent" [dbase, key]

parseGetent :: Parsec String a -> String -> String -> String -> a
parseGetent parser dbase key input =
  case parse parser (unwords ["getent", dbase, key]) input of
    Left  err -> error $ show err
    Right r   -> r


userFromID :: UserID -> Craft (Maybe User)
userFromID = userFromName . show


useradd :: User -> Craft ()
useradd User{..} =
  exec_ "/usr/sbin/useradd" $ args ++ toArg "--gid" (gid group)
 where
  args = Prelude.concat
    [ toArg "--uid"      uid
    , toArg "--comment"  comment
    , toArg "--groups"   $ intercalate "," groups
    , toArg "--home"     home
    , toArg "--password" passwordHash
    , toArg "--shell"    shell
    ]


instance Craftable User where
  watchCraft user = do
    let name = username user
        notFound = $craftError $ "User `" ++ name ++ "` not found!"
    userFromName name >>= \case
      Nothing -> do
        useradd user
        userFromName name >>= \case
          Nothing -> notFound
          Just user' -> do
            verify user' user
            return (Created, user')
      Just user' -> do
        res <- mapM (\(cond, act) -> if cond then return True
                                             else act >> return False)
                 [ (username user' == username user, notImplemented "set username")
                 , (uid user' == uid user, notImplemented "set uid")
                 , (groupname (group user') == groupname (group user), notImplemented "set group")
                 , (groups user' == groups user, notImplemented "set groups")
                 , (home user' == home user, notImplemented "set home")
                 , (passwordHash user' == passwordHash user', notImplemented "set passwordHash")
                 , (shell user' == shell user, notImplemented "set shell")
                 ]
        if and res then
          return (Unchanged, user')
        else
          userFromName name >>= \case
            Nothing -> notFound
            Just user'' -> do
              verify user'' user
              return (Updated, user'')

   where
    verify user' user = notImplemented "verify User"


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
groupFromName gname =
  getent "group" gname >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> return
                  . Just
                  $ parseGetent groupParser "group" gname $ r ^. stdout


groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID = groupFromName . show


instance Craftable Group where
  watchCraft grp = do
    notImplemented "craft Group"
    -- groupFromName . groupname
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" (gid grp) ++ [(groupname grp)]
    exec_ "/usr/bin/gpasswd" ["--members", intercalate "," (members grp), (groupname grp)]
    return (Unchanged, grp)
