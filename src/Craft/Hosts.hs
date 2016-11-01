{-# OPTIONS_GHC -fno-warn-orphans #-}
module Craft.Hosts
( IP(..)
, Name(..)
, Configs
, Hosts(..)
, Craft.Hosts.lookup
, hostsfp
, get
, parseHosts
, showConfigs
, toFile
, fromFile
, insert
, deleteIP
, deleteName
, delete
, set
, setHost
)
where


import           Control.Lens          hiding (noneOf, set)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List             as L

import           Craft
import qualified Craft.File            as File
import           Craft.Hosts.Parser    (parseHosts)
import           Craft.Hosts.Types


lookup :: IP -> Hosts -> Maybe [Name]
lookup ip hosts = L.lookup ip $ configs hosts


hostsMap :: ((IP, [Name]) -> (IP, [Name])) -> Hosts -> Hosts
hostsMap f (Hosts cfgs) = Hosts $ Prelude.map f cfgs


get :: Craft Hosts
get =
  File.getWithContent hostsfp >>= \case
    Nothing -> $craftError $ fromAbsFile hostsfp ++ " not found!"
    Just f  -> case f ^. fileContent of
                 Nothing -> $craftError $ show hostsfp ++ " not found!"
                 Just bs -> parseHosts $ B8.unpack bs


instance Craftable Hosts Hosts where
  watchCraft hosts = do
    (w, f) <- watchCraft $ toFile hosts
    r <- fromFile f
    return (w, r)


showConfigs :: Configs -> String
showConfigs = unlines . map (\(ip, as) -> unwords (show ip:map show as))


toFile :: Hosts -> File
toFile (Hosts cfgs) = file hostsfp & strContent .~ showConfigs cfgs


fromFile :: File -> Craft Hosts
fromFile f =
  case f ^. fileContent of
    Nothing -> return $ Hosts []
    Just c  -> parseHosts $ B8.unpack c


insert :: IP -> Name -> Hosts -> Hosts
insert newip name (Hosts cfgs) = fixUp $ Hosts go
 where
  go | any ((== newip) . fst) cfgs = map f cfgs
     | otherwise           = cfgs ++ [(newip, [name])]
  f (ip, names) | ip == newip && name `notElem` names = (ip, name:names)
                | otherwise                           = (ip, names)


deleteIP :: IP -> Hosts -> Hosts
deleteIP ip (Hosts cfgs) = fixUp . Hosts $ filter (\(ip', _) -> ip' /= ip) cfgs


deleteName :: Name -> Hosts -> Hosts
deleteName name = fixUp . hostsMap f
 where
   f (ip', names) = (ip', filter (/= name) names)


delete :: IP -> Name -> Hosts -> Hosts
delete ip name = fixUp . hostsMap f
 where
  f (ip', names) | ip' == ip = (ip', filter (/= name) names)
                 | otherwise = (ip', names)


set :: Name -> IP -> Hosts -> Hosts
set name ip hosts = fixUp . insert ip name $ deleteName name hosts


setHost :: Name -> IP -> Craft ()
setHost name ip = craft_ =<< (set name ip <$> get)

----------------------------------------
--   ____       _            _        --
--  |  _ \ _ __(_)_   ____ _| |_ ___  --
--  | |_) | '__| \ \ / / _` | __/ _ \ --
--  |  __/| |  | |\ V / (_| | ||  __/ --
--  |_|   |_|  |_| \_/ \__,_|\__\___| --
----------------------------------------
fixUp :: Hosts -> Hosts
fixUp (Hosts cfgs) = Hosts $ filter f  cfgs
 where
  f (IP ip, names) = not (null ip) && not (null names)

