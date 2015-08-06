module Craft.Options where

import           Options.Applicative

data CraftOptions
  = CraftOptions
    { optsSourcePaths :: [FilePath]
    }

craftOptionsFromArgs :: IO CraftOptions
craftOptionsFromArgs = execParser craftOptionsInfo

craftOptionsInfo :: ParserInfo CraftOptions
craftOptionsInfo =
  CraftOptions <$>
    info (helper <*> sourcePaths)
         (fullDesc
           <> progDesc "Craft Configuration Management System"
           <> header "craft")

sourcePaths :: Parser [FilePath]
sourcePaths =
  many $ strOption
          (long "files"
            <> metavar "DIR"
            <> help "Directory to search for source files")
