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

import Control.Concurrent
import System.Environment
import FileSystem
import Conduit
import Control.Applicative (many)
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Binary (encodeFile, decodeFile)
import Data.Conduit.Zlib (ungzip)
import Data.List (intercalate)
import Data.Maybe
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(..))
import Data.Text (Text)
import Distribution.Package (PackageIdentifier(..), PackageName, unPackageName)
import System.FilePath ((</>), takeExtension)
import Distribution.Version (Version, versionNumbers, nullVersion, withinRange)
import GHC.Generics (Generic)
import qualified Distribution.Types.PackageDescription as PackageDescription
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import qualified Distribution.Verbosity as Verbosity
import qualified Data.List as List
import Control.Monad.Except (throwError, liftEither, runExceptT)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, parseGenericPackageDescriptionMaybe, readGenericPackageDescription)
import PackDeps (Newest(..), PackInfo(..), loadNewestFrom, getReverses)
import System.Directory (withCurrentDirectory, listDirectory)
import System.Exit (ExitCode(..))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process.Typed
import qualified Codec.Archive.Tar as Tar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMap
import qualified Data.Scientific as Scientific
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as O
import qualified System.IO.Utf8 as Utf8
import qualified Data.Text.IO.Utf8 as Utf8
import System.IO

doCommand :: Command -> IO ()
doCommand = \case
  DownloadPackageIndex outFile -> do
    downloadPackageIndex outFile
  GenerateBuildMatrix packageIndex package patches excludedPackages outFile prettify overridesPath -> do
    overrides <- fromMaybe (error "Failed to decode package overrides.") <$> Aeson.decodeFileStrict' @[PackageSource] overridesPath
    matrix <- generateBuildMatrix packageIndex package patches excludedPackages overrides
    let serialiseToFile = \a -> if prettify
          then BL.writeFile outFile (Aeson.encodePretty a)
          else Aeson.encodeFile outFile a
    serialiseToFile matrix
  RealiseBuildMatrix matrixJson outFile prettify -> do
    BuildMatrix{..} <- fromMaybe (error ("failed to decode build matrix from " ++ matrixJson)) <$> Aeson.decodeFileStrict' @BuildMatrix matrixJson
    buildReport <- build packageSrc dependencies

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

-- TODO: filter deprecated packages
generateBuildMatrix :: ()
  => FilePath
  -> PackageName
  -> Maybe [PatchFile]
  -> [PackageName]
  -> [PackageSource]
  -> IO BuildMatrix
generateBuildMatrix packageIndex package patches excludedPackages overrides = do
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
        { packageSrc = case findPkg package overrides of
            Just src -> src
            Nothing -> HackageGet package version
        , dependencies = flip map revDeps $ \(name, _) ->
            case findPkg name overrides of
              Just src -> src
              -- we always default to hackage
              Nothing ->
                let PackInfo v _ _ = id
                      $ fromMaybe (error ("package not found: " ++ unPackageName name))
                      $ HMap.lookup name n
                in HackageGet name v
        , ..
        }
  pure matrix

findPkg :: PackageName -> [PackageSource] -> Maybe PackageSource
findPkg name = List.find $ \case
  HackageGet{..} -> package == name
  FetchFromGitHub{..} -> package == name

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
      <*> ( O.strOption
            $ mconcat
            $ [ O.long "overrides"
              , O.help "source overrides"
              , O.metavar "FILEPATH"
              ]
          )

    realise = RealiseBuildMatrix
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
  | GenerateBuildMatrix FilePath PackageName (Maybe [PatchFile]) [PackageName] FilePath Bool FilePath
    -- ^ (packageIndex, package, patches, excludedPackages, output, prettifyOutput, overrides)
    --
    --   construct a build matrix of the reverse dependencies
    --   of @package@ (excluding @excludedPackages@), from
    --   @packageIndex@, applying @patches@ to the source of
    --   @package@, if any. @overrides@ will be applied to
    --   the reverse dependencies.
    --
    --   serialise the build matrix to @output@, prettifying it
    --   if @prettifyOutput@ is set.
  | RealiseBuildMatrix FilePath FilePath Bool
    -- ^ (matrixJson, output, prettifyOutput)
    --
    --   run the build matrix described by @matrixJson@,
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
  | FetchFromGitHub
      { package :: PackageName
      , owner :: String
      , repo :: String
      , rev :: Maybe String
      , subPath :: Maybe FilePath
      }
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

{-
  BL.writeFile "overrides.json" $ Aeson.encodePretty
    [
      FetchFromGitHub
        { package = "fib"
        , owner = "chessai"
        , repo = "fib"
        , rev = Nothing
        , subPath = Nothing
        }
    , FetchFromGitHub
        { package = "coya"
        , owner = "chessai"
        , repo = "coya"
        , rev = Nothing
        , subPath = Nothing
        }
    , FetchFromGitHub
        { package = "ring-buffers"
        , owner = "chessai"
        , repo = "ring-buffers"
        , rev = Nothing
        , subPath = Nothing
        }
    ]
-}

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

buildEnv :: String -> IO a -> IO a
buildEnv envSuffix act = do
  tmpDir <- getCanonicalTemporaryDirectory
  let template = "pump-the-brakes-" ++ envSuffix
  withTempDirectory tmpDir template $ \dir -> do
    withCurrentDirectory dir act

fetchSource :: ()
  => PackageSource
  -> IO ( Either
            (ExitCode, BL.ByteString, BL.ByteString)
            (FilePath, Version)
        )
fetchSource = \case
  HackageGet{..} -> do
    let pkgNameStr = unPackageName package
    let fullPkgName = pkgNameStr ++ "-" ++ showVersion version
    let url = "https://hackage.haskell.org/package/"
          ++ fullPkgName
          ++ "/"
          ++ fullPkgName
          ++ ".tar.gz"
    let srcDir = fullPkgName
    fetchGz url (pkgNameStr ++ ".tar") (Just ".") $ \tarFile -> do
      Tar.extract "." tarFile
      pure $ Right (srcDir, version)
  FetchFromGitHub{..} -> runExceptT $ do
    let srcDir0 = unPackageName package
    let gitUrl = "https://github.com/"
          ++ owner
          ++ "/" ++ repo
    o0@(e0, _, _) <- readProcess $ proc "git" ["clone", gitUrl, srcDir0]
    when (e0 /= ExitSuccess) $ throwError o0
    o1@(e1, _, _) <- liftIO $ withCurrentDirectory srcDir0 $ do
      readProcess $ proc "git" $ ["checkout"] ++
        maybeToList rev
    when (e1 /= ExitSuccess) $ throwError o1
    let srcDir1 = maybe srcDir0 (srcDir0 </>) subPath
    version <- (liftEither =<<) $ liftIO $ withCurrentDirectory srcDir1 $ do
      stuff <- listDirectory "."
      case List.find (\file -> takeExtension file == ".cabal") stuff of
        Nothing -> pure $ Left (ExitFailure 1, "", "No cabal file found in package")
        Just cabalFile -> do
          mgpd <- do
            b <- B.readFile cabalFile
            pure $ parseGenericPackageDescriptionMaybe b

          pure $ case mgpd of
            Nothing -> Left (ExitFailure 1, "", "Failed to parse cabal file")
            Just gpd -> Right $ pkgVersion $ PackageDescription.package $ packageDescription gpd

    -- once we have the version, we need to copy everything over
    -- to the new directory
    let srcDir = srcDir1 ++ "-" ++ showVersion version
    -- could be pretty slow. will have to find out.
    liftIO $ do
      readFsFilter (/= ".git") srcDir1 >>= \case
        File {} -> fail "shouldn't happen"
        Dir _ fs -> writeFs $ Dir srcDir fs
    pure (srcDir, version)

build :: ()
  => PackageSource
  -> [PackageSource]
  -> IO [BuildReport]
build _ [] = pure []
build pkg deps = buildEnv (unPackageName (package pkg)) $ do
  let allPackages = pkg : deps
  srcDirs <- fmap concat $ forM allPackages $ \src -> do
    e <- fetchSource src
    case e of
      Left err -> print err *> pure []
      Right x -> pure [x]

  let cabalProj depFile = do
        writeFile "cabal.project"
        $ unlines
        $    [ "packages: " ++ fst (head srcDirs) ++ "/" ]
          ++ [ "          " ++ depFile            ++ "/" ]

  forM (zip allPackages srcDirs) $ \(p, (srcDir, v)) -> do
    cabalProj srcDir
    putStrLn $ "Building " ++ srcDir ++ "..."
    (e, out, err) <- readProcess $ proc "cabal" ["v2-build", srcDir]
    pure $ BuildReport (package p) v e
      (TE.decodeUtf8 (BL.toStrict out))
      (TE.decodeUtf8 (BL.toStrict err))
