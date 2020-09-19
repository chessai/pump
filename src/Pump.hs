{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language ViewPatterns #-}

module Pump (main) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (ToJSON(..))
import Data.Binary (encodeFile, decodeFile)
import Data.Maybe
import Data.Ord (Down(..))
import Data.String.Conversions (cs)
import Distribution.Package (PackageName, unPackageName)
import Distribution.Version (Version, nullVersion, withinRange)
import PackDeps (Newest(..), PackInfo(..), Reverses, getReverses)
import Streaming (Stream, Of(..))
import System.Directory (withCurrentDirectory, getCurrentDirectory, makeAbsolute, removeDirectoryRecursive)
import System.FilePath (isAbsolute, (</>))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Process.Typed
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Streaming as SB
import qualified Data.ByteString.Streaming.Char8 as SBC8
import qualified Data.HashMap.Strict as HMap
import qualified Data.List as List
import qualified Data.Yaml as Yaml
import qualified Streaming as S
import qualified Streaming.Prelude as S

import Commands
import Fetch
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

  RealiseBuildMatrix dontCheck matrixJson outFile overridesPath -> do
    BuildMatrix{..} <- loadBuildMatrix matrixJson
    overrides <- loadOverrides overridesPath
    build outFile dontCheck packageSrc dependencies overrides

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
    Yaml.decodeFileThrow @_ @[PackageSource] path

loadBuildMatrix :: FilePath -> IO BuildMatrix
loadBuildMatrix path = do
  Yaml.decodeFileThrow @_ @BuildMatrix path

downloadPackageIndex :: ()
  => FilePath
  -> IO ()
downloadPackageIndex outFile = do
  newest <- fetchIndex
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
main = doCommand =<< parseCommand

build :: ()
  => FilePath
  -> Bool
  -> PackageSource
  -> [PackageSource]
  -> [PackageSource]
  -> IO ()
build outFile' _dontCheck pkg deps overrides = do
  outFile <- do
    if isAbsolute outFile'
      then pure outFile'
      else do
        cwd <- getCurrentDirectory
        makeAbsolute (cwd </> outFile')
  tmpDir <- getCanonicalTemporaryDirectory
  let envSuffix = unPackageName (package pkg)
  let template = "pump-the-brakes-" ++ envSuffix
  let allPkgs = pkg : deps
  withTempDirectory tmpDir template $ \dir -> withCurrentDirectory dir $ do
    srcDir <- either (error . show) fst <$> fetchSource pkg

    runResourceT
      $ SB.writeFile outFile
      $ flip SBC8.snoc ']'
      $ SBC8.cons '['
      $ SB.fromChunks
      $ streamSep (BC8.singleton ',')
      $ S.map (cs . Aeson.encode)
      $ mapStreamM
          (\src -> do
              epath <- liftIO $ fetchSource src
              case epath of
                Left (exitCode, (cs -> stdout), (cs -> stderr)) -> do
                  let report = BuildReport
                        { pkg = sourcePackage src
                        , version = sourceVersion src
                        , phase = Fetching
                        , ..
                        }
                  S.yield report
                  let pkgName = sourcePackage src
                  when (isJust (findPkg pkgName overrides)) $ do
                    let err = concat
                          [ "Failed to fetch ", (unPackageName pkgName), ". "
                          , "stderr was: ", cs stderr
                          ]
                    fail err
                Right (p, v) -> do
                  liftIO $ createCabalProject p srcDir

                  buildReport <- liftIO $ do
                    putStrLn $ "Building " ++ p ++ "..."
                    runCabal Building (package src) v
                  S.yield buildReport

                  testReport <- liftIO $ do
                    putStrLn $ "Testing " ++ p ++ "..."
                    runCabal Testing (package src) v
                  S.yield testReport

                  when (p /= srcDir) $ liftIO $ do
                    removeDirectoryRecursive p
          )
      $ S.each
      $ allPkgs

runCabal :: Phase -> PackageName -> Version -> IO BuildReport
runCabal phase p v =
  case phase of
    Fetching -> do
      fail $ "Cabal should not be running during FetchPhase."
    Building -> do
      (e, out, err) <- readProcess $ proc "cabal" ["v2-build", "all"]
      pure $ BuildReport p (Just v) Building e (cs out) (cs err)
    Testing -> do
      (e, out, err) <- readProcess $ proc "cabal" ["v2-test", "all"]
      pure $ BuildReport p (Just v) Testing e (cs out) (cs err)

mapStreamM :: Monad m
  => (a -> Stream (Of b) m x)
  -> Stream (Of a) m r
  -> Stream (Of b) m r
mapStreamM f = id
  . S.concats
  . S.mapsM
      (\(a :> s) -> pure (f a *> pure s)
      )

createCabalProject :: FilePath -> FilePath -> IO ()
createCabalProject depFile srcDir
  | depFile == srcDir = do
      writeFile "cabal.project"
      $ unlines
      $ [ "packages: " ++ srcDir ++ "/"
        ]
  | otherwise = do
      writeFile "cabal.project"
      $ unlines
      $ [ "packages: "
        , "          " ++ srcDir ++ "/"
        , "          " ++ depFile ++ "/"
        ]

serialiseToFile :: ToJSON a => FilePath -> a -> IO ()
serialiseToFile path a =
  Yaml.encodeFile path a

streamSep :: Monad m => B.ByteString -> Stream (Of B.ByteString) m r -> Stream (Of B.ByteString) m r
streamSep sep = go
  where
    go s0 = do
      e <- lift (S.next s0)
      case e of
        Left r -> do
          pure r
        Right (a, rest) -> do
          S.yield a
          prependToAll rest
    prependToAll s = do
      e <- lift (S.next s)
      case e of
        Left r -> do
          pure r
        Right (a, rest) -> do
          S.yield sep
          S.yield a
          prependToAll rest
