module Craft.Daemontools where

import           Craft
import qualified Craft.File as File
import qualified Craft.Directory as Directory
import qualified Craft.Upstart as Upstart

import Control.Lens
import           Data.ByteString (ByteString)


data Service
  = Service
    { _name     :: String
    , _home     :: Directory.Path
    , _env      :: [(String, String)]
    , _runFile  :: ByteString
    , _restarts :: Bool
    }
  deriving (Eq, Show)
makeLenses ''Service


service :: String -> Service
service name' =
  Service { _name     = name'
          , _home     = "/etc/svc"
          , _env      = []
          , _runFile  = ""
          , _restarts = True
          }


setup :: Directory.Path -> Craft ()
setup home' = do
  mapM_ (craft_ . package)
    [ "daemontools"
    , "daemontools-run"
    ]
  craft_ $ directory home'
  Upstart.get "svscan" >>= \case
    Nothing     -> $craftError "Upstart service `svscan` not found!"
    Just svscan -> Upstart.start svscan


path :: Service -> Directory.Path
path Service{..} = _home </> _name


restart :: Service -> Craft ()
restart s = execRestart $ Craft.Daemontools.path s


execRestart :: Directory.Path -> Craft ()
execRestart fp = exec_ "svc" ["-t", fp]


logRunDefault :: FilePath -> String
logRunDefault logdest =
  "#!/bin/sh\nexec setuidgid nobody multilog t " ++ logdest ++ " s10000000 n10\n"


envFile :: Directory.Path -> (String, String) -> File
envFile envDir (varname, varval) =
  file (envDir </> varname) & mode       .~ Mode RW O O
                            & strContent .~ varval


getEnv :: Service -> Craft [(String, String)]
getEnv svc = do
  fs <- Directory.getFiles (Craft.Daemontools.path svc)
  return $ map go fs
 where
  go f = (f ^. File.name, f ^. strContent)


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
