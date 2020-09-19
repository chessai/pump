{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module Fetch
  ( fetchIndex
  , fetchGz
  , fetchSource
  )
  where

import Conduit
import Control.Monad
import Control.Monad.Except (throwError, liftEither, runExceptT)
import Data.Conduit.Zlib (ungzip)
import Data.Maybe
import Distribution.Package (PackageIdentifier(..), unPackageName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription(..))
import Distribution.Version (Version, versionNumbers)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Conduit
import PackDeps (loadNewestFrom)
import PackDeps.Types (Newest)
import System.Directory (withCurrentDirectory, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, (</>))
import System.Process.Typed
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Distribution.Types.PackageDescription as PackageDescription

import FileSystem
import Types

fetchIndex :: IO Newest
fetchIndex = do
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

showVersion :: Version -> String
showVersion = List.intercalate "." . map show . versionNumbers
