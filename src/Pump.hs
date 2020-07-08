{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module Pump (main) where

import Control.Exception (Exception(..), SomeException(..), try)
import Network.HTTP.Conduit
import Conduit
import Data.Conduit.Zlib (ungzip)
import PackDeps (Newest, loadNewestFrom, getReverses)
import Network.HTTP.Client.TLS (getGlobalManager)
import Data.Compact (compact, getCompact)
import Data.Compact.Serialize (writeCompact, unsafeReadCompact)

main :: IO ()
main = do
  -- TODO: use binary, because this sucks
  newest <- loadData
  writeCompact "newest.compact" =<< compact newest
  newest' <- unsafeReadCompact @Newest "newest.compact"
  print $ fmap getCompact newest'

loadData :: IO Newest
loadData = do
  req <- (\r -> r { responseTimeout = responseTimeoutMicro 300_000 })
         <$> parseUrlThrow "http://hackage.haskell.org/01-index.tar.gz"

  res <- try @SomeException @Newest $ do
    m <- getGlobalManager
    runResourceT $ do
      res <- http req m
      runConduit $
           responseBody res
        .| ungzip
        .| sinkFile "tmp"
    loadNewestFrom "tmp"

  either (fail . displayException) pure res
