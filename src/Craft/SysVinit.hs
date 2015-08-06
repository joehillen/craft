module Craft.SysVinit where

import Craft

reload :: FilePath -> Craft ()
reload path = exec_ path ["reload"]
