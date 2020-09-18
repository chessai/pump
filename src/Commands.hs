{-# language LambdaCase #-}

module Commands
  ( Command(..)
  , parseCommand
  ) where

import Control.Applicative ((<|>), many)
import Distribution.Package (PackageName)
import qualified Options.Applicative as O

import Types

data Command
  = DownloadPackageIndex FilePath
    -- ^ (targetFile)
    --
    --   download the package index and serialise it to
    --   @targetFile@ as binary
  | GenerateBuildMatrix FilePath PackageName (Maybe [PatchFile]) [PackageName] FilePath (Maybe FilePath) (Maybe Int)
    -- ^ (packageIndex, package, patches, excludedPackages, output, overrides, n)
    --
    --   construct a build matrix of the reverse dependencies
    --   of @package@ (excluding @excludedPackages@), from
    --   @packageIndex@, applying @patches@ to the source of
    --   @package@, if any. @overrides@ will be applied to
    --   the reverse dependencies.
    --
    --   If @n@ is given, the build matrix will only consider
    --   the top n immediate reverse dependencies of
    --   @package@, sorted by number of reverse dependencies.
    --
    --   serialise the build matrix to @output@.
  | RealiseBuildMatrix Bool FilePath FilePath
    -- ^ (dontCheck, matrixJson, output)
    --
    --   run the build matrix described by @matrixJson@,
    --   and dump the build report to @output@.
    --
    --   @dontCheck@ determines whether or not to run tests.
    --   by default, it is 'False'.
  | Top FilePath PackageName Int
    -- ^ (packageIndex, package, n)
    --
    --   Return a list of the top N most depended on
    --   immediate reverse dependencies of the package.

cmdParser :: O.Parser Command
cmdParser = O.subparser
  $ mconcat
  $ [ O.command "download" (O.info (O.helper <*> download) downloadInfo)
    , O.command "matrix" (O.info (O.helper <*> matrix) matrixInfo)
    , O.command "realise" (O.info (O.helper <*> realise) realiseInfo)
    , O.command "realize" (O.info (O.helper <*> realise) realiseInfo)
    , O.command "top" (O.info (O.helper <*> top) topInfo)
    ]
  where
    downloadInfo = O.fullDesc
    matrixInfo = O.fullDesc
    realiseInfo = O.fullDesc
    topInfo = O.fullDesc

    download = DownloadPackageIndex
      <$> ( O.strOption
            $ mconcat
            $ [ O.long "target"
              , O.short 'o'
              , O.help "where to dump binary package index"
              , O.metavar "FILEPATH"
              ]
          )

    matrix = GenerateBuildMatrix
      <$> ( O.strOption
            $ mconcat
            $ [ O.long "index"
              , O.short 'i'
              , O.help "location of package index"
              , O.metavar "FILEPATH"
              ]
          )
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "pkg"
              , O.short 'p'
              , O.help "name of package"
              , O.metavar "PACKAGE NAME"
              ]
          )
      <*> patchiz
      <*> ( many
              ( O.strOption
                $ mconcat
                $ [ O.long "exclude"
                  , O.short 'e'
                  , O.help "exclude the package from the output"
                  , O.metavar "PACKAGE NAME"
                  ]
              )
          )
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "target"
              , O.short 'o'
              , O.help "where to dump serialised build matrix"
              , O.metavar "FILEPATH"
              ]
          )
      <*> overrides
      <*> (pure Nothing <|> (fmap Just $ O.option O.auto
            $ mconcat
            $ [ O.short 'n'
              , O.help "top N most depended on immediate reverse dependencies"
              , O.metavar "INT"
              ]
          ))

    realise = RealiseBuildMatrix
      <$> ( O.switch
              $ mconcat
              $ [ O.long "dontCheck"
                , O.help "whether or not to run tests"
                ]
          )
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "matrix"
              , O.short 'm'
              , O.help "location of build matrix description"
              , O.metavar "JSON FILEPATH"
              ]
          )
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "target"
              , O.short 'o'
              , O.help "where to dump serialised build report"
              , O.metavar "FILEPATH"
              ]
          )

    top = Top
      <$> ( O.strOption
            $ mconcat
            $ [ O.long "index"
              , O.short 'i'
              , O.help "location of package index"
              , O.metavar "FILEPATH"
              ]
          )
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "pkg"
              , O.short 'p'
              , O.help "name of package"
              , O.metavar "PACKAGE NAME"
              ]
          )
      <*> ( O.option O.auto
            $ mconcat
            $ [ O.short 'n'
              , O.help "Get top N most depended on immediate reverse dependencies"
              , O.metavar "INT"
              ]
          )

    overrides = pure Nothing
      <|> ( fmap Just
            $ O.strOption
            $ mconcat
            $ [ O.long "overrides"
              , O.help "source overrides"
              , O.metavar "FILEPATH"
              ]
          )

    patchiz = id
      $ fmap (\case { [] -> Nothing; xs -> Just xs; })
      $ many
      $ O.strOption
      $ mconcat
      $ [ O.long "patch"
        , O.help "patch file to apply to package source"
        , O.metavar "PATCHFILE"
        ]

parseCommand :: IO Command
parseCommand = O.execParser
  $ O.info (O.helper <*> cmdParser)
  $ mconcat
  $ [ O.fullDesc
    ]

