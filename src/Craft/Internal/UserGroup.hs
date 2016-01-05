module Craft.Internal.UserGroup where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text

import Craft.Internal
import Craft.Internal.Helpers


type UserName = Text
type UserID = Int
type GroupID = Int

data User
  = User
    { _username     :: UserName
    , _uid          :: UserID
    , _comment      :: Text
    , _group        :: Group
    , _groups       :: [GroupName]
    , _home         :: FilePath
    , _passwordHash :: Text
    --, _salt         :: Text
    --, _locked       :: Bool
    , _shell        :: FilePath
    --, system       :: Bool
    }
 deriving (Eq, Show)


type GroupName = Text


data Group
  = Group
    { _groupname :: GroupName
    , _gid       :: GroupID
    , _members   :: [UserName]
    }
  deriving (Eq, Show)


makeLenses ''User
makeLenses ''Group


colon :: Parser Char
colon = char ':'


-- TESTME
passwdParser :: Parser (UserName, UserID, GroupID, Text, FilePath, FilePath)
passwdParser = do
  name <- someTill anyChar colon
  _ <- manyTill anyChar colon
  uid' <- read <$> someTill digitChar colon
  gid' <- read <$> someTill digitChar colon
  comment' <- manyTill anyChar colon
  home' <- manyTill anyChar colon
  shell' <- manyTill anyChar (void eol <|> eof)
  return (T.pack name, uid', gid', T.pack comment', home', shell')


-- TESTME
shadowParser :: Parser Text
shadowParser = do
  _name <- someTill anyChar colon
  T.pack <$> manyTill anyChar colon


userFromName :: UserName -> Craft (Maybe User)
userFromName name =
  getent "passwd" name >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> do
      (nameS, uid', gid', comment', home', shell') <-
            parseGetent passwdParser "passwd" name (r ^. stdout)
      grp <- fromJust <$> groupFromID gid'
      grps <- getGroups nameS
      shadowResult <- $errorOnFail =<< getent "shadow" nameS
      passwordHash' <- parseGetent shadowParser "shadow" nameS $ shadowResult ^. stdout
      return . Just
            $ User { _username     = nameS
                   , _uid          = uid'
                   , _group        = grp
                   , _groups       = grps
                   , _passwordHash = passwordHash'
                   , _home         = home'
                   , _shell        = shell'
                   , _comment      = comment'
                   }

getGroups :: UserName -> Craft [GroupName]
getGroups name = do
  r <- $errorOnFail =<< exec "id" ["-nG", name]
  return $ r ^. stdout . to T.words


getent :: Text -> Text -> Craft ExecResult
getent dbase key = exec "getent" [dbase, key]


parseGetent :: Parsec Text a -> Text -> Text -> Text -> Craft a
parseGetent parser dbase key input =
  case parse parser (T.unpack $ T.unwords ["getent", dbase, key]) input of
    Left  err -> $craftError . T.pack $ show err
    Right r   -> return r


userFromID :: UserID -> Craft (Maybe User)
userFromID = userFromName . T.pack . show


useradd :: User -> Craft ()
useradd User{..} =
  exec_ "/usr/sbin/useradd" $ args ++ toArg "--gid" (_gid _group)
 where
  args = Prelude.concat
    [ toArg "--uid"      _uid
    , toArg "--comment"  _comment
    , toArg "--groups"   $ T.intercalate "," _groups
    , toArg "--home"     _home
    , toArg "--password" _passwordHash
    , toArg "--shell"    _shell
    ]


instance Craftable User where
  watchCraft user = do
    let name = user ^. username
        notFound = $craftError $ "User `" <> name <> "` not found!"
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
                 [ (user' ^. username  == user ^. username, $notImplemented "set username")
                 , (user' ^. uid == user ^. uid, $notImplemented "set uid")
                 , (user' ^. group . groupname == user ^. group . groupname, $notImplemented "set group")
                 , (user' ^. groups == user ^. groups, $notImplemented "set groups")
                 , (user' ^. home == user ^. home, $notImplemented "set home")
                 , (user' ^. passwordHash == user' ^. passwordHash, $notImplemented "set passwordHash")
                 , (user' ^. shell == user ^. shell, $notImplemented "set shell")
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
    verify user' user = $notImplemented "verify User"


-- TESTME
groupParser :: Parser Group
groupParser = do
  name <- someTill anyChar colon
  _ <- manyTill anyChar colon
  gid' <- read <$> someTill digitChar colon
  members' <- some alphaNumChar `sepBy` char ','
  return $ Group (T.pack name) gid' (map T.pack members')


groupFromName :: GroupName -> Craft (Maybe Group)
groupFromName gname =
  getent "group" gname >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> Just <$> parseGetent groupParser "group" gname (r ^. stdout)


groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID = groupFromName . T.pack . show


instance Craftable Group where
  watchCraft grp = do
    $notImplemented "craft Group"
    -- groupFromName . groupname
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" (grp ^. gid) ++ [grp ^. groupname]
    exec_ "/usr/bin/gpasswd" ["--members", T.intercalate "," (grp ^. members), grp ^. groupname]
    return (Unchanged, grp)
