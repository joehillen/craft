module Craft.Nginx where

import           Craft
import           Craft.Internal.Helpers
import           Craft.File (File, file)
import qualified Craft.File as File
import qualified Craft.SysV as SysV

import Control.Lens


setup :: Craft ()
setup =
  craft_ $ package "nginx"


baseDir, sitesDir :: FilePath
baseDir = "/etc/nginx"
sitesDir = baseDir </> "sites-enabled"


data Config
  = Config
    { _configName       :: Name
    , _configDirectives :: [Directive]
    , _configServers    :: Servers
    , _configPriority   :: Int
    }


type Servers   = [Server]


data Server
  = Server
    { _serverNames      :: [Name]
    , _listen           :: (Address, Port, Args)
    , _serverDirectives :: [Directive]
    , _locations        :: [Location]
    }


data SSL
  = SSL
    { _sslKey  :: File
    , _sslCert :: File
    }


data Address
  = Address String
  | AnyAddress


instance Show Address where
  show AnyAddress  = "*"
  show (Address t) = t


type Port      = Int
data Location  = Location Path [Directive] [Location]
type Path      = String
type Directive  = (Name, Args)
type Name       = String


makeLenses ''Config
makeLenses ''Server
makeLenses ''SSL


config :: Name -> Config
config name =
  Config
  { _configName       = name
  , _configPriority   = 10
  , _configDirectives = []
  , _configServers    = []
  }


server :: Server
server =
  Server
    { _serverNames      = []
    , _listen           = (AnyAddress, 80, [])
    , _serverDirectives = []
    , _locations        = []
    }


sslServer :: SSL -> Server
sslServer ssl =
  Server
    { _serverNames      = []
    , _listen           = (AnyAddress, 443, ["ssl"])
    , _serverDirectives =
        [ ("ssl",                 ["on"])
        , ("ssl_certificate",     [ssl ^. sslCert . File.path])
        , ("ssl_certificate_key", [ssl ^. sslKey . File.path])
        , ("ssl_session_cache",   ["shared:SSL:10m"])
        , ("ssl_session_timeout", ["5m"])
        , ("ssl_protocols",       ["TLSv1", "TLSv1.1", "TLSv1.2"])
        , ("ssl_ciphers",         ["ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA"])
        , ("ssl_prefer_server_ciphers", ["on"])
        ]
    , _locations = []
    }



redirectWWWtoNoWWW :: [Server] -> [Server]
redirectWWWtoNoWWW servers =
  map go servers ++ servers
 where
  go s =
    s & serverNames .~ map ("www." ++) (s ^. serverNames)
      & locations   .~ []
      & serverDirectives .~ s ^. serverDirectives ++
        [ ("return", ["301", "http://" ++ s ^. serverNames . _head ++ "$uri"]) ]


logs :: FilePath -> String -> [Directive]
logs logdir name =
  [ ("access_log", [ logprefix ++ ".access.log", "combined"])
  , ("error_log", [ logprefix ++ ".error.log" ])
  ]
 where
  logprefix = logdir </> name


root :: [Directive] -> Location
root dirs = Location "/" dirs []


instance Show Config where
  show cfg = dirs ++ servers
   where
    dirs = concatMap showDirective $ cfg ^. configDirectives
    servers = concatMap show $ cfg ^. configServers


showDirective :: Directive -> String
showDirective (name, args) =
  name ++ " " ++ unwords args ++ ";"


instance Show Server where
  show Server{..} = "\n" ++
    "server {\n" ++ indent 2 (
      "listen " ++ showListen _listen ++ ";\n" ++
      "server_name " ++ unwords _serverNames ++ ";\n" ++
      unlines ( map showDirective _serverDirectives ++ map show _locations)) ++
    "}\n"


instance Show Location where
  show (Location path dirs sublocations) =
    "location " ++ path ++ " {\n" ++ indent 2 (
        unlines $ map showDirective dirs
               ++ map show sublocations) ++
    "}\n"


showListen :: (Address, Port, Args) -> String
showListen (addr, port, args) =
  show addr ++ ":" ++ show port ++ " " ++ unwords args


toFile :: Config -> File
toFile c =
  file (sitesDir </> show (c ^. configPriority) ++ "_" ++ c ^. configName ++ ".conf")
    & File.strContent .~ show c


reload :: Craft ()
reload = SysV.reload $ SysV.service "nginx"
