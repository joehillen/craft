module Craft.Daemontools where

import           Craft
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.File.Link (Link(Link))
import           Craft.Directory (directory)
import qualified Craft.Directory as Directory
import qualified Craft.User as User
import qualified Craft.Group as Group
import qualified Craft.Upstart as Upstart

import           Control.Monad (when)
import           Data.Maybe
import           Data.ByteString (ByteString)

setup :: Directory.Path -> Craft ()
setup home = do
  mapM_ (craft . package)
    [ "daemontools"
    , "daemontools-run"
    ]

  craft_ $ directory home

  svscan <- Upstart.get "svscan"
  Upstart.start $ fromJust svscan

path :: Service -> Directory.Path
path Service{..} = home </> name

restart :: Service -> Craft ()
restart s = execRestart $ path s

execRestart :: Directory.Path -> Craft ()
execRestart fp = exec_ "/usr/bin/svc" ["-t", fp]

data Service
  = Service
    { name     :: String
    , home     :: Directory.Path
    , env      :: [(String, String)]
    , runFile  :: ByteString
    , restarts :: Bool
    }
  deriving (Eq, Show)

logRunDefault :: FilePath -> String
logRunDefault logdest =
  "#!/bin/sh\nexec setuidgid nobody multilog t " ++ logdest ++ " s10000000 n10\n"

service :: String -> Service
service name =
  Service
  { name     = name
  , home     = "/etc/svc"
  , env      = []
  , runFile  = ""
  , restarts = True
  }

envFile :: Directory.Path -> (String, String) -> File
envFile envDir (en, ev) =
  (file $ envDir </> en)
    { File.mode    = Mode RW O O
    , File.content = File.strContent ev
    }

getEnv :: Service -> Craft [(String, String)]
getEnv svc = do
  fs <- Directory.getFiles (path svc)
  return $ map go fs
 where
  go f = (File.name f, File.contentAsString f)

{-
instance Craftable Service where
  checker svc = File.get (path svc </> "run") >>= \case
    Nothing -> return Nothing
    Just  f -> do
      env' <- getEnv svc
      return . Just $
        svc { runFile = fromJust $ File.content f
            , env = env'
            }

  crafter s@Service{..} _ = do
    craft_ $ directory home

    let svcdir = path s
    craft_ $ directory svcdir
    craft_ $ Link svcdir $ "/etc/service" </> name

    runW <- fst <$> watchCraft ((file $ svcdir </> "run")
                                { File.mode    = Mode RWX RX RX
                                , File.content = Just runFile
                                })

    let logDir = svcdir </> "log"
    craft_ $ directory logDir

    let logDest = logDir </> "main"
    nobody  <- fromJust <$> User.fromName "nobody"
    nogroup <- fromJust <$> Group.fromName "nogroup"
    craft_ $ (directory logDest)
                { Directory.ownerID = User.uid nobody
                , Directory.groupID = Group.gid nogroup
                }

    let envDir = svcdir </> "env"
    craft_ $ directory envDir
    mapM_ (craft . envFile envDir) env

    when (restarts && updated runW) $ restart s
-}
