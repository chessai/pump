{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}
{-# language TypeApplications #-}

{-# options_ghc -fno-warn-orphans #-}

module Types
  ( BuildMatrix(..)
  , PackageSource(..)
  , PatchFile
  , BuildReport(..)
  , Phase(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import Distribution.Package (PackageName)
import Distribution.Version (Version)
import GHC.Generics (Generic)
import PackDeps ()
import System.Exit (ExitCode(..))
import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Scientific

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

data Phase = Building | Testing
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data BuildReport = BuildReport
  { pkg :: PackageName
  , version :: Version
  , phase :: Phase
  , exitCode :: ExitCode
  , stdout :: Text
  , stderr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSON ExitCode where
  toJSON = \case
    ExitSuccess -> toJSON @Int 0
    ExitFailure n -> toJSON n

instance FromJSON ExitCode where
  parseJSON = Aeson.withScientific "ExitCode" $ \s -> case Scientific.toBoundedInteger s of
    Nothing -> fail "not a bounded Int"
    Just 0 -> pure ExitSuccess
    Just n -> pure (ExitFailure n)
