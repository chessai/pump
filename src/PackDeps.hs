{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}
{-# language ViewPatterns #-}

module PackDeps
    ( Newest
    , Reverses

    , loadNewestFrom
    , getReverses
    ) where

import Control.Exception (throw)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Distribution.Package (PackageName, Dependency(..), mkPackageName, unPackageName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (runParseResult, parseGenericPackageDescription)
import Distribution.Text (simpleParse)
import Distribution.Types.CondTree (CondBranch (..))
import Distribution.Version (Version, VersionRange, withinRange)
import PackDeps.Types
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Text as T

loadNewestFrom :: FilePath -> IO Newest
loadNewestFrom = fmap parseNewest . L.readFile

parseNewest :: L.ByteString -> Newest
parseNewest = id
  . fst
  . foldl' addPackage (Newest HMap.empty, 0)
  . entriesToList
  . Tar.read

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList = \case
  Tar.Done -> []
  Tar.Fail s -> throw s
  Tar.Next e es -> e : entriesToList es

addPackage :: (Newest, Int) -> Tar.Entry -> (Newest, Int)
addPackage (Newest m, count) entry = (Newest m', count')
  where
    (m', count') = case splitOn "/" (Tar.fromTarPathToPosixPath (Tar.entryTarPath entry)) of
      -- we don't care about acme packages
      (packageName : _) | "acme" `T.isPrefixOf` T.pack packageName -> (m, count)
      [packageStr, versionStr, nameStr] | ".cabal" `T.isSuffixOf` T.pack nameStr ->
        let packageName = mkPackageName packageStr
         in case simpleParse versionStr of
              Nothing -> (m, count)
              Just newv -> case HMap.lookup packageName m of
                Nothing -> go packageName newv
                Just PackInfo { version = oldv } ->
                  if newv >= oldv
                  then go packageName newv
                  else (m, count)
      _ -> (m, count)
    go :: PackageName
       -> Version
       -> (HashMap PackageName PackInfo, Int)
    go p v = case Tar.entryContent entry of
      Tar.NormalFile bs _ ->
        let packInfo = PackInfo
              { version = v
              , desc = getDeps bs
              , epoch = Tar.entryTime entry
              }
        in (HMap.insert p packInfo m, count + 1)
      _ -> (m, count)

getReverses :: Newest -> Reverses
getReverses (Newest newest) = HMap.fromList withVersion
  where
    toTuples (_, PackInfo { desc = Nothing }) = HMap.empty
    toTuples (rel, PackInfo { desc = Just deps })
      -- TODO: ignore deprecated packages (requires looking at synposis)
      -- | isDeprecated deps = HMap.empty
      | otherwise = combine $ map (toTuple rel) $ HMap.toList deps
    combine = unionsWith HMap.union

    toTuple rel (dep, PUVersionRange _ range)
      | rel == dep = HMap.empty
      | otherwise = HMap.singleton dep (HMap.singleton rel range)

    hoisted :: HashMap PackageName (HashMap PackageName VersionRange)
    hoisted = combine $ map toTuples $ HMap.toList newest

    withVersion = mapMaybe addVersion $ HMap.toList hoisted

    addVersion (dep, rels) = case HMap.lookup dep newest of
      Nothing -> Nothing
      Just (PackInfo { version = v }) -> Just (dep, (v, rels))

unionsWith :: (Foldable f, Hashable k, Eq k)
  => (v -> v -> v)
  -> f (HashMap k v)
  -> HashMap k v
unionsWith f = foldl' (HMap.unionWith f) HMap.empty

--isDeprecated ::
{-
maxVersion :: Ord v => PackInfo n v l -> PackInfo n v l -> PackInfo n v l
maxVersion pi1 pi2 = if piVersion pi1 <= piVersion pi2 then pi2 else pi1

getReverses :: Newest -> Reverses
getReverses (Newest newest) =
    HMap.fromList withVersion
  where
    -- dep = dependency, rel = relying package
    --toTuples :: (PackageName, PackInfo) -> HMap.HashMap PackageName (HMap.HashMap PackageName VersionRange)
    toTuples (_, PackInfo { piDesc = Nothing' }) = HMap.empty
    toTuples (rel, PackInfo { piDesc = Just' desc@DescInfo { diDeps = deps } })
        -- | isDeprecated desc = HMap.empty
        | otherwise = combine $ map (toTuple rel) $ HMap.toList deps

    combine = unionsWith HMap.union

    toTuple rel (dep, PUVersionRange _ range) =
        if rel == dep
            then HMap.empty
            else HMap.singleton dep $ HMap.singleton rel range

    hoisted :: HMap.HashMap PackageName (HMap.HashMap PackageName (VersionRange Version))
    hoisted = combine $ map toTuples $ HMap.toList newest

    withVersion = mapMaybe addVersion $ HMap.toList hoisted

    addVersion (dep, rels) =
        case HMap.lookup dep newest of
            Nothing -> Nothing
            Just PackInfo { piVersion = v} -> Just (dep, (v, rels))

getDescInfo :: GenericPackageDescription -> (DescInfo PackageName Version, License)
getDescInfo gpd = (DescInfo
    { diHaystack = toCaseFold $ pack $ unlines [author p, maintainer p, name]
    , diDeps = getDeps gpd
    , diSynopsis = pack $ synopsis p
    }, License $ pack $ prettyShow $ license $ packageDescription gpd)
  where
    p = packageDescription gpd
    PackageIdentifier (D.unPackageName -> name) _version = package p
-}

getDeps :: L.ByteString -> Maybe (HashMap PackageName PUVersionRange)
getDeps lbs = do
  gpd <- id $ either (const Nothing) Just
            $ snd
            $ runParseResult
            $ parseGenericPackageDescription
            $ L.toStrict lbs
  let flagMaps =
        let loop = \case
              [] -> pure Map.empty
              (f : fs) -> do
                let name = flagName f
                let def = flagDefault f
                rest <- loop fs
                [Map.insert name def rest, Map.insert name (not def) rest]

        in take 10 . loop . genPackageFlags $ gpd

      allowsNewBase :: [Dependency] -> Bool
      allowsNewBase = all ok
        where
          Just newbase = simpleParse "4.10.0.0"

          ok :: Dependency -> Bool
          ok (Dependency (unPackageName -> "base") range _)
            = newbase `withinRange` range
          ok _ = True

      go' fm tree = id
        $ concat
        $ condTreeConstraints tree
        : map (go' fm) (mapMaybe (checkCond fm) (condTreeComponents tree))

      checkCond fm (CondBranch cond tree melse)
        | checkCond' fm cond = Just tree
        | otherwise = melse


      checkCond' fm = \case
        Var (OS _) -> True
        Var (Arch _) -> True
        Var (Flag f) -> Map.findWithDefault False f fm
        Var (Impl _ _) -> True
        Lit b -> b
        CNot c -> not (checkCond' fm c)
        COr c1 c2 -> checkCond' fm c1 || checkCond' fm c2
        CAnd c1 c2 -> checkCond' fm c1 && checkCond' fm c2

      go :: PackageUsage
         -> CondTree ConfVar [Dependency] a
         -> [(Dependency, PackageUsage)]
      go pu tree = map (,pu)
        $ case filter allowsNewBase choices of
            [] -> case choices of
              [] -> []
              (c : _) -> c
            (c : _) -> c
        where
          choices = map (flip go' tree) flagMaps

  pure $ foldr (HMap.unionWith (<>)) HMap.empty
       $ map (\(Dependency k v _, pu) -> HMap.singleton
                 k
                 (PUVersionRange pu v)
             )
       $ mconcat
       $ [ maybe mempty (map (,Runtime) . setupDepends)
             $ setupBuildInfo $ packageDescription gpd
         , maybe mempty (go Runtime) (condLibrary gpd)
         , foldMap (go Runtime . snd) (condSubLibraries gpd)
         , foldMap (go Runtime . snd) (condForeignLibs gpd)
         , foldMap (go Runtime . snd) (condExecutables gpd)
         , foldMap (go TestBench . snd) (condTestSuites gpd)
         , foldMap (go TestBench . snd) (condBenchmarks gpd)
         ]

{-
checkDeps :: Newest
          -> (PackageName, Version, DescInfo PackageName Version)
          -> (PackageName, Version, CheckDepsRes)
checkDeps newest (name, version, desc) =
    case mapMaybe (notNewest newest) $ HMap.toList $ diDeps desc of
        [] -> (name, version, AllNewest)
        x -> let y = HMap.fromList $ map fst x
                 et = maximum $ map snd x
              in (name, version, WontAccept y $ epochToTime et)

-- | Whether or not a package can accept all of the newest versions of its
-- dependencies. If not, it returns a list of packages which are not accepted,
-- and a timestamp of the most recently updated package.
data CheckDepsRes = AllNewest
                  | WontAccept (HMap.HashMap PackageName Outdated) UTCTime
    deriving Show

data Outdated = Outdated Version Reason

instance Show Outdated where
    show (Outdated _ Deprecated) = "deprecated"
    show (Outdated version NewerAvailable) = show version
    show (Outdated version NewerAndDeprecated) = show version ++ " (deprecated)"

data Reason = NewerAvailable | Deprecated | NewerAndDeprecated
    deriving Show

epochToTime :: Tar.EpochTime -> UTCTime
epochToTime e = addUTCTime (fromIntegral e) $ UTCTime (read "1970-01-01") 0

notNewest :: Newest
          -> (PackageName, PUVersionRange (VersionRange Version))
          -> Maybe ((PackageName, Outdated), Tar.EpochTime)
notNewest (Newest newest) (s, PUVersionRange _ range) =
    case HMap.lookup s newest of
        --Nothing -> Just ((s, " no version found"), 0)
        Nothing -> Nothing
        Just PackInfo { piVersion = version, piEpoch = e, piDesc = d } ->
            let mreason =
                    case (maybe' False isDeprecated d, not $ withinRange version range) of
                        (False, False) -> Nothing
                        (True, False) -> Just Deprecated
                        (False, True) -> Just NewerAvailable
                        (True, True) -> Just NewerAndDeprecated
             in flip fmap mreason $ \reason -> ((s, Outdated version reason), e)

-- | Loads up the newest version of a package from the 'Newest' list, if
-- available.
getPackage :: PackageName -> Newest -> Maybe (PackageName, Version, DescInfo PackageName Version)
getPackage s (Newest n) = do
    pi <- HMap.lookup s n
    di <- m'ToM $ piDesc pi
    return (s, piVersion pi, di)
-}

{-
-- | Load a single package from a cabal file.
loadPackage :: FilePath -> IO (Maybe' (DescInfo PackageName Version))
loadPackage = fmap (fmap fst . parsePackage) . L.readFile

isDeprecated :: DescInfo name version -> Bool
isDeprecated desc = "(deprecated)" `isInfixOf` diSynopsis desc

-- | Find all of the packages matching a given search string.
filterPackages :: Text -> Newest -> [(PackageName, Version, DescInfo PackageName Version)]
filterPackages needle =
    mapMaybe go . HMap.toList . unNewest
  where
    go (name, PackInfo { piVersion = v, piDesc = Just' desc }) =
        if matches (diHaystack desc) &&
           not (isDeprecated desc)
            then Just (name, v, desc)
            else Nothing
    go _ = Nothing

    matches haystack
        | Just needle' <- TS.stripPrefix "exact:" needle = all (`elem` TS.words haystack) $ TS.words $ toCaseFold needle'
        | otherwise =
            let (needle', excludes) = splitExcludes $ toCaseFold needle
             in (needle' `isInfixOf` haystack) && all (\t -> not $ t `isInfixOf` haystack) excludes

    splitExcludes = second (filter (not . TS.null) . TS.split (== '!'))
                  . TS.break (== '!')

-- | Find all packages depended upon by the given list of packages.
deepDeps :: Newest
         -> [(PackageName, Version, DescInfo PackageName Version)]
         -> [(PackageName, Version, DescInfo PackageName Version)]
deepDeps (Newest newest) dis0 =
    go Set.empty dis0
  where
    go _ [] = []
    go viewed ((name, v, di):dis)
        | name `Set.member` viewed = go viewed dis
        | otherwise = (name, v, di) : go viewed' (newDis ++ dis)
      where
        viewed' = Set.insert name viewed
        newDis = mapMaybe getDI $ HMap.keys $ diDeps di
        getDI name' = do
            pi <- HMap.lookup name' newest
            di' <- m'ToM $ piDesc pi
            return (name', piVersion pi, di')

data LMS = LMS
    { _lmsProcessed :: Set.Set PackageName
    , _lmsToProcess :: [PackageName]
    , _lmsResult :: LicenseMap
    }
makeLenses ''LMS

getLicenseMap :: Bool -- ^ include test/benchmarks
              -> Newest -> LicenseMap
getLicenseMap includeTests (Newest newest) =
    evalState go (LMS Set.empty (HMap.keys newest) Map.empty)
  where
    go = do
        lms <- get
        case lms ^. lmsToProcess of
            [] -> return $ lms ^. lmsResult
            p:rest -> do
                lmsToProcess %= const rest
                _ <- getLicenses p
                go

    getLicenses :: PackageName -> State LMS Licenses
    getLicenses p = do
        lms1 <- get
        if p `Set.member` (lms1 ^. lmsProcessed)
            then return $ fromMaybe mempty $ Map.lookup p $ lms1 ^. lmsResult
            else do
                lmsProcessed %= Set.insert p
                case HMap.lookup p newest of
                    Nothing -> return mempty
                    Just pi -> do
                        let ls1 = Licenses $ Map.singleton (piLicense pi) $ Set.singleton p
                            deps =
                                case piDesc pi of
                                    Nothing' -> []
                                    Just' di -> map fst $ filter isIncluded $ HMap.toList $ diDeps di
                        lss <- mapM getLicenses deps
                        let ls = mconcat $ ls1 : lss
                        lmsResult %= Map.insert p ls
                        return ls

    isIncluded (_, PUVersionRange Runtime _) = True
    isIncluded (_, PUVersionRange TestBench _) = includeTests
-}
