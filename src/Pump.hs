{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Pump (main) where

import Conduit
import Control.Monad
import Control.Monad.Except (throwError, liftEither, runExceptT)
import Data.Aeson (ToJSON(..))
import Data.Binary (encodeFile, decodeFile)
import Data.Conduit.Zlib (ungzip)
import Data.List (intercalate)
import Data.Maybe
import Data.Ord (Down(..))
import Distribution.Package (PackageIdentifier(..), PackageName, unPackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(..))
import Distribution.Version (Version, versionNumbers, nullVersion, withinRange)
import FileSystem
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import PackDeps (Newest(..), PackInfo(..), Reverses, loadNewestFrom, getReverses)
import System.Directory (withCurrentDirectory, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeExtension)
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process.Typed
import qualified Codec.Archive.Tar as Tar
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import qualified Distribution.Types.PackageDescription as PackageDescription

import Commands
import Types

doCommand :: Command -> IO ()
doCommand = \case

  DownloadPackageIndex outFile -> do
    downloadPackageIndex outFile

  GenerateBuildMatrix packageIndex package patches excludedPackages outFile overridesPath n -> do
    overrides <- loadOverrides overridesPath
    newest <- decodeFile @Newest packageIndex
    let matrix = generateBuildMatrix newest package patches excludedPackages overrides n
    serialiseToFile outFile matrix

  RealiseBuildMatrix dontCheck matrixJson outFile -> do
    BuildMatrix{..} <- loadBuildMatrix matrixJson
    buildReport <- build dontCheck packageSrc dependencies
    serialiseToFile outFile buildReport
  Top packageIndex package n -> do
    newest <- decodeFile @Newest packageIndex
    let reverses = getReverses newest
    let tops = topDependencies reverses package n
    forM_ tops (putStrLn . unPackageName)

loadOverrides :: Maybe FilePath -> IO [PackageSource]
loadOverrides = \case
  Nothing -> do
    pure []
  Just path -> do
    fromMaybe (error "Failed to decode package overrides.")
      <$> Aeson.decodeFileStrict' @[PackageSource] path

loadBuildMatrix :: FilePath -> IO BuildMatrix
loadBuildMatrix path = do
  fromMaybe err <$> Aeson.decodeFileStrict' @BuildMatrix path
  where
    err = error
      ("failed to decode build matrix from " ++ path)

serialiseToFile :: ToJSON a => FilePath -> a -> IO ()
serialiseToFile path a =
  BL.writeFile path (Aeson.encodePretty a)

downloadPackageIndex :: ()
  => FilePath
  -> IO ()
downloadPackageIndex outFile = do
  newest <- loadNewest
  encodeFile outFile newest

getPackagesForMatrix :: ()
  => Reverses
  -> PackageName
  -> [PackageName]
  -> Maybe Int
  -> (Version, [PackageName])
getPackagesForMatrix reverses package excludedPackages = \case
  Nothing ->
    case HMap.lookup package reverses of
      Nothing -> (nullVersion, [])
      Just (packageVersion, revDeps0) ->
        let keepPackage (pkg, rng) = True
              && pkg `notElem` excludedPackages
              && withinRange packageVersion rng
            revDeps1 = id
              $ map fst
              $ filter keepPackage
              $ HMap.toList revDeps0
        in (packageVersion, revDeps1)
  Just n ->
    let version = maybe nullVersion fst $ HMap.lookup package reverses
        revDeps = id
          $ filter (`notElem` excludedPackages)
          $ topDependencies reverses package n
    in (version, revDeps)

-- TODO: filter deprecated packages
generateBuildMatrix :: ()
  => Newest
  -> PackageName
  -> Maybe [PatchFile]
  -> [PackageName]
  -> [PackageSource]
  -> Maybe Int
  -> BuildMatrix
generateBuildMatrix newest package patches excludedPackages overrides n =
  let reverses = getReverses newest
      (version, revDeps) = getPackagesForMatrix reverses package excludedPackages n

  in BuildMatrix
    { packageSrc = case findPkg package overrides of
        Just src -> src
        Nothing -> HackageGet package version
    , dependencies = flip map revDeps $ \name ->
        case findPkg name overrides of
          Just src -> src
          -- we always default to hackage
          Nothing ->
            let PackInfo v _ _ = id
                  $ fromMaybe (error ("package not found: " ++ unPackageName name))
                  $ HMap.lookup name (getNewest newest)
            in HackageGet name v
    , ..
    }

topDependencies :: ()
  => Reverses
  -> PackageName
  -> Int
  -> [PackageName]
topDependencies reverses package n = id
  $ List.take n
  $ map fst
  $ List.sortOn (Down . snd)
  $ mapMaybe
      (\p -> do
          h <- HMap.lookup p reverses
          pure (p, HMap.size (snd h))
      )
  $ directDependencies package reverses

directDependencies :: PackageName -> Reverses -> [PackageName]
directDependencies package reverses = id
  $ HMap.keys
  $ maybe (error (show package ++ ": package doesn't exist")) snd
  $ HMap.lookup package
  $ reverses

findPkg :: PackageName -> [PackageSource] -> Maybe PackageSource
findPkg name = List.find $ \case
  HackageGet{..} -> package == name
  FetchFromGitHub{..} -> package == name

main :: IO ()
main = do
  cmd <- parseCommand
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
    pure (req0 { responseTimeout = responseTimeoutMicro 3_000_000 })
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
    liftIO $ print o1
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
  => Bool
  -> PackageSource
  -> [PackageSource]
  -> IO [BuildReport]
build _ _ [] = pure []
build dontCheck pkg deps = buildEnv (unPackageName (package pkg)) $ do
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

  fmap List.concat $ forM (zip allPackages srcDirs) $ \(p, (srcDir, v)) -> do
    cabalProj srcDir
    putStrLn $ "Building " ++ srcDir ++ "..."
    buildReport <- do
      (e, out, err) <- readProcess $ proc "cabal" ["v2-build", srcDir]
      pure $ BuildReport (package p) v Building e
        (TE.decodeUtf8 (BL.toStrict out))
        (TE.decodeUtf8 (BL.toStrict err))
    testReport <- if dontCheck then pure [] else fmap (:[]) $ do
      (e, out, err) <- readProcess $ proc "cabal" ["v2-test", srcDir]
      pure $ BuildReport (package p) v Testing e
        (TE.decodeUtf8 (BL.toStrict out))
        (TE.decodeUtf8 (BL.toStrict err))
    pure (buildReport : testReport)
