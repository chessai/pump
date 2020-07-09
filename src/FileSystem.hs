{-# language DerivingStrategies #-}
{-# language LambdaCase #-}

module FileSystem
  ( FileSystem(..)
  , readFs
  , writeFs
  ) where

import Control.Monad
import Data.Foldable (for_)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, withCurrentDirectory, listDirectory, doesFileExist)

import qualified Data.Text.IO as TIO

data FileSystem
  = File FilePath Text
  | Dir FilePath [FileSystem]
  deriving stock (Eq, Show)

writeFs :: FileSystem -> IO ()
writeFs = \case
  File name content -> do
    TIO.writeFile name content
  Dir name children -> do
    createDirectoryIfMissing False name
    withCurrentDirectory name $ for_ children writeFs

readFs :: FilePath -> IO FileSystem
readFs sourceDir = withCurrentDirectory sourceDir $ do
  lsOutput <- listDirectory "."
  fs <- fmap concat $ forM lsOutput $ \name -> do
    isFile <- doesFileExist name
    if isFile
      then do
        contents <- TIO.readFile name
        pure [File name contents]
      else do
        fs <- readFs name
        pure [fs]
  pure (Dir sourceDir fs)
