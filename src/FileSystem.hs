{-# language DerivingStrategies #-}
{-# language LambdaCase #-}

module FileSystem
  ( FileSystem(..)
  , readFs
  , readFsFilter
  , writeFs
  ) where

import Control.Monad
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory, listDirectory, doesFileExist)

import qualified Data.ByteString as B

data FileSystem
  = File FilePath ByteString
  | Dir FilePath [FileSystem]
  deriving stock (Eq, Show)

writeFs :: FileSystem -> IO ()
writeFs = \case
  File name content -> do
    B.writeFile name content
  Dir name children -> do
    createDirectoryIfMissing False name
    withCurrentDirectory name $ for_ children writeFs

readFs :: FilePath -> IO FileSystem
readFs = readFsFilter (const True)

readFsFilter :: (FilePath -> Bool) -> FilePath -> IO FileSystem
readFsFilter p sourceDir = withCurrentDirectory sourceDir $ do
  lsOutput <- filter p <$> listDirectory "."
  fs <- fmap concat $ forM lsOutput $ \name -> do
    isFile <- doesFileExist name
    if isFile
      then do
        contents <- B.readFile name
        pure [File name contents]
      else do
        fs <- readFs name
        pure [fs]
  pure (Dir sourceDir fs)

