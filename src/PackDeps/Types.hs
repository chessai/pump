{-# language DerivingStrategies #-}

-- for hashable instances
{-# options_ghc -fno-warn-orphans #-}

module PackDeps.Types
  ( Newest(..)
  , Reverses
  , PackInfo(..)
  , PUVersionRange(..)
  , PackageUsage(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable(..))
import Data.Int (Int64)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.VersionRange.Internal (VersionRange(..))
import Distribution.Utils.ShortText (ShortText, fromShortText)
import Distribution.Version (Version, simplifyVersionRange)

instance Hashable PackageName where
  hashWithSalt i = hashWithSalt i . unPackageName
instance Hashable ShortText where
  hashWithSalt i = hashWithSalt i . fromShortText

newtype Newest = Newest (HashMap PackageName PackInfo)
  deriving stock (Show)

type Reverses = HashMap PackageName (Version, HashMap PackageName VersionRange)

data PackInfo = PackInfo
  { version :: Version
  , desc :: Maybe (HashMap PackageName PUVersionRange)
  , epoch :: Int64
  }
  deriving stock (Show)

data PUVersionRange = PUVersionRange PackageUsage VersionRange
  deriving stock (Show)

instance Semigroup PUVersionRange where
  PUVersionRange a x <> PUVersionRange b y
    = PUVersionRange
      (a <> b)
      (simplifyVersionRange $ IntersectVersionRanges x y)

data PackageUsage = Runtime | TestBench
  deriving stock (Show)

instance Semigroup PackageUsage where
  Runtime   <> _ = Runtime
  TestBench <> x = x

instance Monoid PackageUsage where
  mempty = TestBench
