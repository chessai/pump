{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Pump (main) where

import Conduit
import Control.Applicative (many)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Binary (encodeFile, decodeFile)
import Data.Conduit.Zlib (ungzip)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Distribution.Package (PackageName, unPackageName)
import Distribution.Version (Version, versionNumbers, nullVersion, withinRange)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import PackDeps (Newest(..), PackInfo(..), loadNewestFrom, getReverses)
import qualified Codec.Archive.Tar as Tar
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O

doCommand :: Command -> IO ()
doCommand = \case
  DownloadPackageIndex outFile -> do
    downloadPackageIndex outFile
  GenerateBuildMatrix packageIndex package patches excludedPackages outFile -> do
    matrix <- generateBuildMatrix packageIndex package patches excludedPackages
    Aeson.encodeFile outFile matrix

downloadPackageIndex :: ()
  => FilePath
  -> IO ()
downloadPackageIndex outFile = do
  newest <- loadNewest
  encodeFile outFile newest

generateBuildMatrix :: ()
  => FilePath
  -> PackageName
  -> Maybe [PatchFile]
  -> [PackageName]
  -> IO BuildMatrix
generateBuildMatrix packageIndex package patches excludedPackages = do
  newest@(Newest n) <- decodeFile @Newest packageIndex
  let (version, revDeps) = case HMap.lookup package (getReverses newest) of
        Nothing -> (nullVersion, [])
        Just (packageVersion, revDeps0) ->
          let keepPackage (pkg, rng) = True
                && pkg `notElem` excludedPackages
                && withinRange packageVersion rng
              revDeps1 = id
                $ filter keepPackage
                $ HMap.toList revDeps0
          in (packageVersion, revDeps1)

  let matrix = BuildMatrix
        { packageSrc = CabalGet package version
        , dependencies = flip map revDeps $ \(name, _) ->
            let PackInfo v _ _ = id
                  $ fromMaybe (error ("package not found: " ++ unPackageName name))
                  $ HMap.lookup name n
            in CabalGet name v
        , ..
        }
  pure matrix

cmdParser :: O.Parser Command
cmdParser = O.subparser
  $ mconcat
  $ [ O.command "download" (O.info (O.helper <*> download) downloadInfo)
    , O.command "matrix" (O.info (O.helper <*> matrix) matrixInfo)
    ]
  where
    downloadInfo = O.fullDesc
    matrixInfo = O.fullDesc

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
      <*> patches
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

    patches = fmap (\case { [] -> Nothing; xs -> Just xs; })
      $ many
      $ O.strOption
      $ mconcat
      $ [ O.long "patch"
        , O.help "patch file to apply to package source"
        , O.metavar "PATCHFILE"
        ]

data Command
  = DownloadPackageIndex FilePath
    -- ^ (targetFile)
    --
    --   download the package index and serialise it to
    --   @targetFile@ as binary
  | GenerateBuildMatrix FilePath PackageName (Maybe [PatchFile]) [PackageName] FilePath
    -- ^ (packageIndex, package, patches, excludedPackages, output)
    --
    --   construct a build matrix of the reverse dependencies
    --   of @package@ (excluding @excludedPackages@), from
    --   @packageIndex@, applying @patches@ to the source of
    --   @package@, if any.
    --
    --   serialise the build matrix to @output@

data BuildMatrix = BuildMatrix
  { packageSrc :: PackageSource
  , patches :: Maybe [PatchFile]
  , dependencies :: [PackageSource]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PackageSource
  = CabalGet
      { package :: PackageName
      , version :: Version
      }
  | FetchFromGitHub
      { owner :: Text
      , repo :: Text
      , revision :: Text
      , subPath :: FilePath
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

{-
showVersion :: Version -> String
showVersion = intercalate "." . map show . versionNumbers

fetchSource :: PackageSource -> IO FileSystem
fetchSource = \case
  CabalGet{..} -> do
    let pkgNameStr = unPackageName package
    let fullPkgName = pkgNameStr ++ "-" ++ showVersion version
    let url = "https://hackage.haskell.org/package/"
          ++ fullPkgName
          ++ "/"
          ++ fullPkgName
          ++ ".tar.gz"

    tmpDir <- getCanonicalTemporaryDirectory
    withTempDirectory tmpDir pkgNameStr $ \dir -> do
      withCurrentDirectory dir $ do
        let srcDir = fullPkgName
        fetchGz url (pkgNameStr ++ ".tar") (Just dir) $ \tarFile -> do
          createDirectoryIfMissing False srcDir
          Tar.extract srcDir tarFile
          readFs srcDir
  FetchFromGitHub{..} -> do
    pure undefined
-}

type PatchFile = FilePath

main :: IO ()
main = do
  cmd <- O.execParser
    $ O.info (O.helper <*> cmdParser)
    $ mconcat
    $ [ O.fullDesc
      ]
  doCommand cmd

loadNewest :: IO Newest
loadNewest = do
  let url = "http://hackage.haskell.org/01-index.tar.gz"
  fetchGz url "newest-index.tar" Nothing $ \index -> do
    newest <- loadNewestFrom index
    pure newest

  --(index, indexHandle) <- openTempFile tmpDir "newest-index.tar"
  --fetchGz  index
  --newest <- loadNewestFrom index
  --hClose indexHandle
  --pure newest

fetchGz :: String
        -> FilePath
        -> Maybe FilePath
        -> (FilePath -> IO a)
        -> IO a
fetchGz url outFile mtmpDir fromFile = do
  req <- do
    req0 <- parseUrlThrow url
    pure (req0 { responseTimeout = responseTimeoutMicro 300_000 })
  m <- getGlobalManager

  let sink = case mtmpDir of
        Nothing -> sinkSystemTempFile
        Just tmpDir -> sinkTempFile tmpDir

  runResourceT $ do
    res <- http req m
    streamedFile <- runConduit $
         responseBody res
      .| ungzip
      .| sink outFile
    liftIO $ fromFile streamedFile
