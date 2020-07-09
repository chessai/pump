{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-orphans #-}

module Pump (main) where

import Conduit
import Control.Applicative (many)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..))
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
import System.Directory (withCurrentDirectory, createDirectoryIfMissing, listDirectory)
import System.Exit (ExitCode(..))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process.Typed
import qualified Codec.Archive.Tar as Tar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMap
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as TE
import qualified Options.Applicative as O

doCommand :: Command -> IO ()
doCommand = \case
  DownloadPackageIndex outFile -> do
    downloadPackageIndex outFile
  GenerateBuildMatrix packageIndex package patches excludedPackages outFile prettify -> do
    matrix <- generateBuildMatrix packageIndex package patches excludedPackages
    let serialiseToFile = \a -> if prettify
          then BL.writeFile outFile (Aeson.encodePretty a)
          else Aeson.encodeFile outFile a
    serialiseToFile matrix
  EnactBuildMatrix matrixJson outFile prettify -> do
    BuildMatrix{ packageSrc = HackageGet p v, .. } <- fromMaybe (error ("failed to decode build matrix from " ++ matrixJson)) <$> Aeson.decodeFileStrict' @BuildMatrix matrixJson
    buildReport <- build p v (map (\(HackageGet pd vd) -> (pd, vd)) dependencies)

    let serialiseToFile = \a -> if prettify
          then BL.writeFile outFile (Aeson.encodePretty a)
          else Aeson.encodeFile outFile a
    serialiseToFile buildReport

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
        { packageSrc = HackageGet package version
        , dependencies = flip map revDeps $ \(name, _) ->
            let PackInfo v _ _ = id
                  $ fromMaybe (error ("package not found: " ++ unPackageName name))
                  $ HMap.lookup name n
            in HackageGet name v
        , ..
        }
  pure matrix

cmdParser :: O.Parser Command
cmdParser = O.subparser
  $ mconcat
  $ [ O.command "download" (O.info (O.helper <*> download) downloadInfo)
    , O.command "matrix" (O.info (O.helper <*> matrix) matrixInfo)
    , O.command "realise" (O.info (O.helper <*> realise) realiseInfo)
    , O.command "realize" (O.info (O.helper <*> realise) realiseInfo)
    ]
  where
    downloadInfo = O.fullDesc
    matrixInfo = O.fullDesc
    realiseInfo = O.fullDesc

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
      <*> ( O.switch
            $ mconcat
            $ [ O.long "prettify"
              , O.help "prettify output"
              ]
          )

    realise = EnactBuildMatrix
      <$> ( O.strOption
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
      <*> ( O.switch
            $ mconcat
            $ [ O.long "prettify"
              , O.help "prettify output"
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
  | GenerateBuildMatrix FilePath PackageName (Maybe [PatchFile]) [PackageName] FilePath Bool
    -- ^ (packageIndex, package, patches, excludedPackages, output, prettifyOutput)
    --
    --   construct a build matrix of the reverse dependencies
    --   of @package@ (excluding @excludedPackages@), from
    --   @packageIndex@, applying @patches@ to the source of
    --   @package@, if any.
    --
    --   serialise the build matrix to @output@, prettifying it
    --   if @prettifyOutput@ is set.
  | EnactBuildMatrix FilePath FilePath Bool
    -- ^ (matrixJson, output, prettifyOutput)
    --
    --   Run the build matrix described by @matrixJson@,
    --   and dump the build report to @output@, prettifying it
    --   if @prettifyOutput@ is set.
    --

data BuildMatrix = BuildMatrix
  { packageSrc :: PackageSource
  , patches :: Maybe [PatchFile]
  , dependencies :: [PackageSource]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PackageSource
  = HackageGet
      { package :: PackageName
      , version :: Version
      }
--  | FetchFromGitHub
--      { owner :: Text
--      , repo :: Text
--      , revision :: Text
--      , subPath :: FilePath
--      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

showVersion :: Version -> String
showVersion = intercalate "." . map show . versionNumbers

instance ToJSON ExitCode where
  toJSON = \case
    ExitSuccess -> toJSON @Int 0
    ExitFailure n -> toJSON n

instance FromJSON ExitCode where
  parseJSON = Aeson.withScientific "ExitCode" $ \s -> case Scientific.toBoundedInteger s of
    Nothing -> fail "not a bounded Int"
    Just 0 -> pure ExitSuccess
    Just n -> pure (ExitFailure n)

data BuildReport = BuildReport
  { pkg :: PackageName
  , version :: Version
  , exitCode :: ExitCode
  , stdout :: Text
  , stderr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

build :: ()
  => PackageName
  -> Version
  -> [(PackageName, Version)]
  -> IO [BuildReport]
build _ _ [] = pure []
build name version deps = do
  tmpDir <- getCanonicalTemporaryDirectory
  let template = "pump-the-brakes-" ++ unPackageName name
  let allPackages = (name, version) : deps
  withTempDirectory tmpDir template $ \dir -> do
    withCurrentDirectory dir $ do
      forM_ allPackages $ \(n, v) -> do
        let pkgNameStr = unPackageName n
        let fullPkgName = pkgNameStr ++ "-" ++ showVersion v
        let url = "https://hackage.haskell.org/package/"
              ++ fullPkgName
              ++ "/"
              ++ fullPkgName
              ++ ".tar.gz"
        let srcDir = fullPkgName
        fetchGz url (pkgNameStr ++ ".tar") (Just ".") $ \tarFile -> do
          Tar.extract "." tarFile
          pure srcDir

      let cabalProj headFile depFile =
            writeFile "cabal.project"
            $ unlines
            $    [ "packages: " ++ headFile ++ "/" ]
              ++ [ "          " ++ depFile ++ "/" ]

      forM allPackages $ \(n, v) -> do
        let srcDir = unPackageName n ++ "-" ++ showVersion v
        cabalProj (unPackageName name ++ "-" ++ showVersion version) srcDir
        (e, out, err) <- readProcess $ proc "cabal" ["build", srcDir]
        pure $ BuildReport n v e
          (TE.decodeUtf8 (BL.toStrict out))
          (TE.decodeUtf8 (BL.toStrict err))
