{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Internal.UserGroup where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.String

import Craft.Internal
import Craft.Internal.Helpers
import Craft.Internal.Helpers.Parsing


colon :: Parser Char
colon = char ':'


-- TESTME
passwdParser :: Parser (UserName, UserID, GroupID, String, FilePath, FilePath)
passwdParser = do
  name      <- UserName <$> someTill anyChar colon
  _password <- manyTill anyChar colon
  uid'      <- UserID . read <$> someTill digitChar colon
  gid'      <- GroupID . read <$> someTill digitChar colon
  comment'  <- manyTill anyChar colon
  home'     <- manyTill anyChar colon
  shell'    <- manyTill anyChar end
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
      return . Just $ User { _userName     = nameS
                           , _uid          = uid'
                           , _userGroup        = grp
                           , _userGroups       = grps
                           , _userPasswordHash = passwordHash'
                           , _userHome         = home'
                           , _userShell        = shell'
                           , _userComment      = comment'
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
  exec_ "useradd" $ args ++ toArg "--gid" (_gid _userGroup)
 where
  args = Prelude.concat
    [ toArg "--uid"      _uid
    , toArg "--comment"  _userComment
    , toArg "--groups"   $ intercalate "," $ map show _userGroups
    , toArg "--home"     _userHome
    , toArg "--password" _userPasswordHash
    , toArg "--shell"    _userShell
    ]


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


