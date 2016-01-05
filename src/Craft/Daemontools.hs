module Craft.Daemontools where

import           Craft
import           Craft.File (File, file)
import qualified Craft.File as File
import           Craft.File.Mode
import           Craft.Directory (directory)
import qualified Craft.Directory as Directory
import qualified Craft.Upstart as Upstart

import Control.Lens
import           Data.ByteString (ByteString)


data Service
  = Service
    { _name     :: Text
    , _home     :: Directory.Path
    , _env      :: [(Text, Text)]
    , _runFile  :: ByteString
    , _restarts :: Bool
    }
  deriving (Eq, Show)
makeLenses ''Service


service :: Text -> Service
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
restart s = execRestart $ path s


execRestart :: Directory.Path -> Craft ()
execRestart fp = exec_ "/usr/bin/svc" ["-t", fp]


logRunDefault :: FilePath -> Text
logRunDefault logdest =
  "#!/bin/sh\nexec setuidgid nobody multilog t " ++ logdest ++ " s10000000 n10\n"


envFile :: Directory.Path -> (Text, Text) -> File
envFile envDir (varname, varval) =
  file (envDir </> varname) & File.mode       .~ Mode RW O O
                            & File.strContent .~ varval


getEnv :: Service -> Craft [(Text, Text)]
getEnv svc = do
  fs <- Directory.getFiles (path svc)
  return $ map go fs
 where
  go f = (f ^. File.name, f ^. File.strContent)


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
