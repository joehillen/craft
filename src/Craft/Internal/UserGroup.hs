module Craft.Internal.UserGroup where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.String

import Craft.Internal
import Craft.Internal.Helpers


newtype UserName = UserName String
                   deriving (Eq, Ord)
newtype UserID = UserID Int
                 deriving (Eq, Show, Ord)
newtype GroupName = GroupName String
                    deriving (Eq, Ord)
newtype GroupID = GroupID Int
                  deriving (Eq, Show, Ord)

instance Show GroupName where
  show (GroupName n) = n

instance Show UserName where
  show (UserName n) = n

instance ToArg UserID where
  toArg arg (UserID n) = [arg, show n]

instance ToArg GroupID where
  toArg arg (GroupID n) = [arg, show n]

data User
  = User
    { _username     :: UserName
    , _uid          :: UserID
    , _comment      :: String
    , _group        :: Group
    , _groups       :: [GroupName]
    , _home         :: FilePath
    , _passwordHash :: String
    --, _salt         :: String
    --, _locked       :: Bool
    , _shell        :: FilePath
    --, system       :: Bool
    }
 deriving (Eq, Show)


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
passwdParser :: Parser (UserName, UserID, GroupID, String, FilePath, FilePath)
passwdParser = do
  name <- UserName <$> someTill anyChar colon
  _ <- manyTill anyChar colon
  uid' <- UserID . read <$> someTill digitChar colon
  gid' <- GroupID . read <$> someTill digitChar colon
  comment' <- manyTill anyChar colon
  home' <- manyTill anyChar colon
  shell' <- manyTill anyChar (void eol <|> eof)
  return (name, uid', gid', comment', home', shell')


-- TESTME
shadowParser :: Parser String
shadowParser = do
  _name <- someTill anyChar colon
  manyTill anyChar colon


userFromStr :: String -> Craft (Maybe User)
userFromStr nameOrIdStr =
  getent "passwd" nameOrIdStr >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> do
      (nameS, uid', gid', comment', home', shell') <-
            parseGetent passwdParser "passwd" nameOrIdStr (r ^. stdout)
      grp <- fromJust <$> groupFromID gid'
      grps <- getGroups nameS
      s <- $stdoutOrError =<< getent "shadow" (show nameS)
      passwordHash' <- parseGetent shadowParser "shadow" (show nameS) s
      return . Just $ User { _username     = nameS
                           , _uid          = uid'
                           , _group        = grp
                           , _groups       = grps
                           , _passwordHash = passwordHash'
                           , _home         = home'
                           , _shell        = shell'
                           , _comment      = comment'
                           }

getGroups :: UserName -> Craft [GroupName]
getGroups (UserName name) = do
  s <- $stdoutOrError =<< exec "id" ["-nG", name]
  return . map GroupName $ words s


getent :: String -> String -> Craft ExecResult
getent dbase key = exec "getent" [dbase, key]


parseGetent :: Parsec String a -> String -> String -> String -> Craft a
parseGetent parser dbase key input =
  case parse parser (unwords ["getent", dbase, key]) input of
    Left  err -> $craftError $ show err
    Right r   -> return r


userFromID :: UserID -> Craft (Maybe User)
userFromID (UserID n) = userFromStr $ show n


useradd :: User -> Craft ()
useradd User{..} =
  exec_ "/usr/sbin/useradd" $ args ++ toArg "--gid" (_gid _group)
 where
  args = Prelude.concat
    [ toArg "--uid"      _uid
    , toArg "--comment"  _comment
    , toArg "--groups"   $ intercalate "," $ map show _groups
    , toArg "--home"     _home
    , toArg "--password" _passwordHash
    , toArg "--shell"    _shell
    ]


instance Craftable User where
  watchCraft user = do
    let name = show $ user ^. username
        notFound = $craftError $ "User `" ++ name ++ "` not found!"
    userFromStr name >>= \case
      Nothing -> do
        useradd user
        userFromStr name >>= \case
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
          userFromStr (show name) >>= \case
            Nothing -> notFound
            Just user'' -> do
              verify user'' user
              return (Updated, user'')

   where
    verify :: User -> User -> Craft ()
    verify _ _ = $notImplemented "verify User"


-- TESTME
groupParser :: Parser Group
groupParser = do
  name <- GroupName <$> someTill anyChar colon
  _ <- manyTill anyChar colon
  gid' <- GroupID . read <$> someTill digitChar colon
  members' <- map UserName <$> some alphaNumChar `sepBy` char ','
  return $ Group name gid' members'


groupFromStr :: String -> Craft (Maybe Group)
groupFromStr nameOrIdStr =
  getent "group" nameOrIdStr >>= \case
    ExecFail _ -> return Nothing
    ExecSucc r -> Just <$> parseGetent groupParser "group" nameOrIdStr (r ^. stdout)


groupFromID :: GroupID -> Craft (Maybe Group)
groupFromID (GroupID n )= groupFromStr $ show n


instance Craftable Group where
  watchCraft grp = do
    _ <- $notImplemented "craft Group"
    -- groupFromName . groupname
    exec_ "/usr/sbin/groupadd" $ toArg "--gid" (grp ^. gid) ++ [show $ grp ^. groupname]
    exec_ "/usr/bin/gpasswd" ["--members", intercalate "," (map show (grp ^. members))
                             , show $ grp ^. groupname]
    return (Unchanged, grp)
