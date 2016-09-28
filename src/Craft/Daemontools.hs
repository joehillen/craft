module Craft.Daemontools where

import           Control.Lens
import           Data.ByteString (ByteString)

import           Craft
import qualified Craft.Upstart   as Upstart


data Service
  = Service
    { _name     :: String
    , _home     :: Path Abs Dir
    , _env      :: [(String, String)]
    , _runFile  :: ByteString
    , _restarts :: Bool
    }
  deriving (Eq, Show)
makeLenses ''Service


service :: String -> Service
service name' =
  Service { _name     = name'
          , _home     = $(mkAbsDir "/etc/svc")
          , _env      = []
          , _runFile  = ""
          , _restarts = True
          }


setup :: Path Abs Dir -> Craft ()
setup home' = do
  craft_ $ map package [ "daemontools"
                       , "daemontools-run"
                       ]
  craft_ $ directory home'
  Upstart.get "svscan" >>= \case
    Nothing     -> $craftError "Upstart service `svscan` not found!"
    Just svscan -> Upstart.start svscan


path :: Service -> Craft (Path Abs Dir)
path Service{..} = do
  sn <- parseRelDir _name
  return $ _home </> sn


restart :: Service -> Craft ()
restart s = do
  sp <- Craft.Daemontools.path s
  execRestart sp


execRestart :: Path Abs Dir -> Craft ()
execRestart fp = exec_ "svc" ["-t", fromAbsDir fp]


logRunDefault :: Path Abs FileP -> String
logRunDefault logdest =
  "#!/bin/sh\nexec setuidgid nobody multilog t "++fromAbsFile logdest++" s10000000 n10\n"


envFile :: Path Abs Dir -> (String, String) -> Craft File
envFile envDir (varname, varval) = do
  fn <- parseRelFile varname
  return $ file (envDir</>fn)
           & mode       .~ Mode RW O O
           & strContent .~ varval


{-
getEnv :: Service -> Craft [(String, String)]
getEnv svc = do
  sp <- Craft.Daemontools.path svc
  fs <- Directory.getFiles sp
  return $ map go fs
 where
  go f = (f ^. File.name, f ^. strContent)


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
