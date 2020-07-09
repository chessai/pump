{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingStrategies #-}
{-# language GeneralizedNewtypeDeriving #-}

-- for hashable instances
{-# options_ghc -fno-warn-orphans #-}

module PackDeps.Types
  ( Newest(..)
  , Reverses
  , PackInfo(..)
  , PUVersionRange(..)
  , PackageUsage(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import Data.Binary (Binary)
import Data.Binary.Instances.UnorderedContainers ()
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Distribution.Text (simpleParse)
import Distribution.Types.PackageName (PackageName, unPackageName, mkPackageName)
import Distribution.Types.VersionRange.Internal (VersionRange(..))
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Version (Version, simplifyVersionRange, versionNumbers)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as T

instance Hashable PackageName where
  hashWithSalt i = hashWithSalt i . unPackageName
instance Hashable ShortText where
  hashWithSalt i = hashWithSalt i . fromShortText

instance ToJSON Version where
  toJSON = id
    . String
    . T.pack
    . List.intercalate "."
    . map show
    . versionNumbers

instance FromJSON Version where
  parseJSON = Aeson.withText "Version" $ \t -> case simpleParse (T.unpack t) of
    Nothing -> fail "Version"
    Just v -> pure v

instance ToJSON PackageName where
  toJSON = toJSON . unPackageName

instance FromJSON PackageName where
  parseJSON = fmap mkPackageName . parseJSON

newtype Newest = Newest (HashMap PackageName PackInfo)
  deriving stock (Show)
  deriving newtype (Eq, Binary)

type Reverses = HashMap PackageName (Version, HashMap PackageName VersionRange)

data PackInfo = PackInfo
  { version :: Version
  , desc :: Maybe (HashMap PackageName PUVersionRange)
  , epoch :: Int64
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data PUVersionRange = PUVersionRange PackageUsage VersionRange
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

instance Semigroup PUVersionRange where
  PUVersionRange a x <> PUVersionRange b y
    = PUVersionRange
      (a <> b)
      (simplifyVersionRange $ IntersectVersionRanges x y)

data PackageUsage = Runtime | TestBench
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

instance Semigroup PackageUsage where
  Runtime   <> _ = Runtime
  TestBench <> x = x

instance Monoid PackageUsage where
  mempty = TestBench
