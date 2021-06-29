{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BenchLib
   where

import Prelude hiding (Int)
import Control.Applicative
import Control.Arrow (first, second)
import Control.DeepSeq (NFData, force)
import Control.Exception (bracket, evaluate)
import Control.Monad (void, unless, guard, (>=>), when)
import Data.Data (Typeable)
import Data.Foldable (foldMap, traverse_)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.List (intercalate, stripPrefix, isPrefixOf, genericLength, genericDrop)
import Data.Monoid (All(..), Any(..))
import Data.Proxy
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Traversable (forM)
import Data.Word (Word64)
import GHC.Conc
import GHC.IO.Encoding
import GHC.Stats
import System.CPUTime
import System.Mem
import Text.Printf
import System.Exit
import System.IO
import System.IO.Unsafe

predictPerturbed :: Measurement -> Measurement -> Estimate
predictPerturbed t1 t2 = Estimate
  { estMean = estMean (predict t1 t2)
  , estStdev = max
    (estStdev (predict (lo t1) (hi t2)))
    (estStdev (predict (hi t1) (lo t2)))
  }
  where
    prec = max (fromInteger cpuTimePrecision) 1000000000 -- 1 ms
    hi meas = meas { measTime = measTime meas + prec }
    lo meas = meas { measTime = measTime meas - prec }

hasGCStats :: Bool
#if MIN_VERSION_base(4,10,0)
hasGCStats = unsafePerformIO getRTSStatsEnabled
#elif MIN_VERSION_base(4,6,0)
hasGCStats = unsafePerformIO getGCStatsEnabled
#else
hasGCStats = False
#endif

getAllocsAndCopied :: IO (Word64, Word64, Word64)
getAllocsAndCopied = do
  if not hasGCStats then pure (0, 0, 0) else
#if MIN_VERSION_base(4,10,0)
    (\s -> (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s)) <$> getRTSStats
#elif MIN_VERSION_base(4,6,0)
    (\s -> (int64ToWord64 $ bytesAllocated s, int64ToWord64 $ bytesCopied s, int64ToWord64 $ peakMegabytesAllocated s * 1024 * 1024)) <$> getGCStats
#else
    pure (0, 0, 0)
#endif

measure :: Word64 -> Benchmarkable -> IO Measurement
measure n (Benchmarkable act) = do
  performGC
  startTime <- fromInteger <$> getCPUTime
  (startAllocs, startCopied, startMaxMemInUse) <- getAllocsAndCopied
  act n
  endTime <- fromInteger <$> getCPUTime
  (endAllocs, endCopied, endMaxMemInUse) <- getAllocsAndCopied
  pure $ Measurement
    { measTime   = endTime - startTime
    , measAllocs = endAllocs - startAllocs
    , measCopied = endCopied - startCopied
    , measMaxMem = max endMaxMemInUse startMaxMemInUse
    }

data Timeout = NoTimeout | Timeout Integer String

data Estimate = Estimate
  { estMean  :: !Measurement
  , estStdev :: !Word64  -- ^ stdev in picoseconds
  } deriving (Show, Read)

data Measurement = Measurement
  { measTime   :: !Word64 -- ^ time in picoseconds
  , measAllocs :: !Word64 -- ^ allocations in bytes
  , measCopied :: !Word64 -- ^ copied bytes
  , measMaxMem :: !Word64 -- ^ max memory in use
  } deriving (Show, Read)

measureUntil :: Bool -> Timeout -> RelStDev -> Benchmarkable -> IO Estimate
measureUntil _ _ (RelStDev targetRelStDev) b
  | isInfinite targetRelStDev, targetRelStDev > 0 = do
  t1 <- measure 1 b
  pure $ Estimate { estMean = t1, estStdev = 0 }
measureUntil warnIfNoTimeout timeout (RelStDev targetRelStDev) b = do
  t1 <- measure 1 b
  go 1 t1 0
  where
    go :: Word64 -> Measurement -> Word64 -> IO Estimate
    go n t1 sumOfTs = do
      t2 <- measure (2 * n) b

      let Estimate (Measurement meanN allocN copiedN maxMemN) stdevN = predictPerturbed t1 t2
          isTimeoutSoon = case timeout of
            NoTimeout -> False
            -- multiplying by 12/10 helps to avoid accidental timeouts
            Timeout micros _ -> (sumOfTs' + 3 * measTime t2) `quot` (1000000 * 10 `quot` 12) >= fromInteger micros
          isStDevInTargetRange = stdevN < truncate (max 0 targetRelStDev * word64ToDouble meanN)
          scale = (`quot` n)
          sumOfTs' = sumOfTs + measTime t1

      case timeout of
        NoTimeout | warnIfNoTimeout, sumOfTs' + measTime t2 > 100 * 1000000000000
          -> hPutStrLn stderr "This benchmark takes more than 100 seconds. Consider setting --timeout, if this is unexpected (or to silence this warning)."
        _ -> pure ()

      if isStDevInTargetRange || isTimeoutSoon
        then pure $ Estimate
          { estMean  = Measurement (scale meanN) (scale allocN) (scale copiedN) maxMemN
          , estStdev = scale stdevN }
        else go (2 * n) t2 sumOfTs'

bench s b = do
  r <- measureUntil False NoTimeout defaultRelStValue b
  putStrLn (s ++ ": " ++ showPicos4 (measTime (estMean r)))

-- | An internal routine to measure execution time in seconds
-- for a given timeout (put 'NoTimeout', or 'mkTimeout' 100000000 for 100 seconds)
-- and a target relative standard deviation
-- (put 'RelStDev' 0.05 for 5% or 'RelStDev' (1/0) to run only one iteration).
--
-- 'Timeout' takes soft priority over 'RelStDev': this function prefers
-- to finish in time even if at cost of precision. However, timeout is guidance
-- not guarantee: 'measureCpuTime' can take longer, if there is not enough time
-- to run at least thrice or an iteration takes unusually long.
measureCpuTime :: Timeout -> RelStDev -> Benchmarkable -> IO Double
measureCpuTime
    = ((fmap ((/ 1e12) . word64ToDouble . measTime . estMean) .) .)
    . measureUntil False

newtype Benchmarkable = Benchmarkable
  { unBenchmarkable :: Word64 -> IO () -- ^ Run benchmark given number of times.
  } deriving (Typeable)

newtype RelStDev = RelStDev Double
  deriving (Show, Read, Typeable)

defaultRelStValue = RelStDev 0.05

{-
-- | In addition to @--stdev@ command-line option,
-- one can adjust target relative standard deviation
-- for individual benchmarks and groups of benchmarks
-- using 'adjustOption' and 'localOption'.
--
-- E. g., set target relative standard deviation to 2% as follows:
--
-- > import Test.Tasty (localOption)
-- > localOption (RelStDev 0.02) (bgroup [...])
--
-- If you set 'RelStDev' to infinity,
-- a benchmark will be executed
-- only once and its standard deviation will be recorded as zero.
-- This is rather a blunt approach, but it might be a necessary evil
-- for extremely long benchmarks. If you wish to run all benchmarks
-- only once, use command-line option @--stdev@ @Infinity@.
--
newtype RelStDev = RelStDev Double
  deriving (Show, Read, Typeable)

defaultRelStValue = RelStDev 0.05

newtype FailIfSlower = FailIfSlower Double
  deriving (Show, Read, Typeable)

defaultFailValue = FailIfSlower (1.0 / 0.0)

newtype FailIfFaster = FailIfFaster Double
  deriving (Show, Read, Typeable)

defaultFailIfFasterValue = FailIfFaster (1.0 / 0.0)

parsePositivePercents :: String -> Maybe Double
parsePositivePercents xs = do
  x <- safeRead xs
  guard (x > 0)
  pure (x / 100)

-- | Something that can be benchmarked, produced by 'nf', 'whnf', 'nfIO', 'whnfIO',
-- 'nfAppIO', 'whnfAppIO' below.
--
-- Drop-in replacement for 'Criterion.Benchmarkable' and 'Gauge.Benchmarkable'.
--

-- | Show picoseconds, fitting number in 3 characters.
showPicos3 :: Word64 -> String
showPicos3 i
  | t < 995   = printf "%3.0f ps" t
  | t < 995e1 = printf "%3.1f ns" (t / 1e3)
  | t < 995e3 = printf "%3.0f ns" (t / 1e3)
  | t < 995e4 = printf "%3.1f μs" (t / 1e6)
  | t < 995e6 = printf "%3.0f μs" (t / 1e6)
  | t < 995e7 = printf "%3.1f ms" (t / 1e9)
  | t < 995e9 = printf "%3.0f ms" (t / 1e9)
  | otherwise = printf "%4.2f s"  (t / 1e12)
  where
    t = word64ToDouble i


showBytes :: Word64 -> String
showBytes i
  | t < 1000                 = printf "%3.0f B " t
  | t < 10189                = printf "%3.1f KB" (t / 1024)
  | t < 1023488              = printf "%3.0f KB" (t / 1024)
  | t < 10433332             = printf "%3.1f MB" (t / 1048576)
  | t < 1048051712           = printf "%3.0f MB" (t / 1048576)
  | t < 10683731149          = printf "%3.1f GB" (t / 1073741824)
  | t < 1073204953088        = printf "%3.0f GB" (t / 1073741824)
  | t < 10940140696372       = printf "%3.1f TB" (t / 1099511627776)
  | t < 1098961871962112     = printf "%3.0f TB" (t / 1099511627776)
  | t < 11202704073084108    = printf "%3.1f PB" (t / 1125899906842624)
  | t < 1125336956889202624  = printf "%3.0f PB" (t / 1125899906842624)
  | t < 11471568970838126592 = printf "%3.1f EB" (t / 1152921504606846976)
  | otherwise                = printf "%3.0f EB" (t / 1152921504606846976)
  where
    t = word64ToDouble i

data Measurement = Measurement
  { measTime   :: !Word64 -- ^ time in picoseconds
  , measAllocs :: !Word64 -- ^ allocations in bytes
  , measCopied :: !Word64 -- ^ copied bytes
  , measMaxMem :: !Word64 -- ^ max memory in use
  } deriving (Show, Read)

data Estimate = Estimate
  { estMean  :: !Measurement
  , estStdev :: !Word64  -- ^ stdev in picoseconds
  } deriving (Show, Read)

data Response = Response
  { respEstimate :: !Estimate
  , respIfSlower :: !FailIfSlower -- ^ saved value of --fail-if-slower
  , respIfFaster :: !FailIfFaster -- ^ saved value of --fail-if-faster
  } deriving (Show, Read)

prettyEstimate :: Estimate -> String
prettyEstimate (Estimate m stdev) =
  showPicos4 (measTime m)
  ++ (if stdev == 0 then "         " else " ± " ++ showPicos3 (2 * stdev))

prettyEstimateWithGC :: Estimate -> String
prettyEstimateWithGC (Estimate m stdev) =
  showPicos4 (measTime m)
  ++ (if stdev == 0 then ",          " else " ± " ++ showPicos3 (2 * stdev) ++ ", ")
  ++ showBytes (measAllocs m) ++ " allocated, "
  ++ showBytes (measCopied m) ++ " copied, "
  ++ showBytes (measMaxMem m) ++ " peak memory"

csvEstimate :: Estimate -> String
csvEstimate (Estimate m stdev) = show (measTime m) ++ "," ++ show (2 * stdev)

csvEstimateWithGC :: Estimate -> String
csvEstimateWithGC (Estimate m stdev) = show (measTime m) ++ "," ++ show (2 * stdev)
  ++ "," ++ show (measAllocs m) ++ "," ++ show (measCopied m) ++ "," ++ show (measMaxMem m)





instance IsTest Benchmarkable where
  testOptions = pure
    [ Option (Proxy :: Proxy RelStDev)
    -- FailIfSlower and FailIfFaster must be options of a test provider rather
    -- than options of an ingredient to allow setting them on per-test level.
    , Option (Proxy :: Proxy FailIfSlower)
    , Option (Proxy :: Proxy FailIfFaster)
    ]
  run opts b = const $ case getNumThreads (lookupOption opts) of
    1 -> do
      pure $ testPassed $ show (Response est (lookupOption opts) (lookupOption opts))
    _ -> pure $ testFailed "Benchmarks must not be run concurrently. Please pass -j1 and/or avoid +RTS -N."


-- | Attach a name to 'Benchmarkable'.
--
-- This is actually a synonym of 'Test.Tasty.Providers.singleTest'
-- to provide an interface compatible with 'Criterion.bench' and 'Gauge.bench'.
--
bench :: String -> Benchmarkable -> Benchmark
bench = singleTest

-- | Attach a name to a group of 'Benchmark'.
--
-- This is actually a synonym of 'Test.Tasty.testGroup'
-- to provide an interface compatible with 'Criterion.bgroup'
-- and 'Gauge.bgroup'.
--
bgroup :: String -> [Benchmark] -> Benchmark
bgroup = testGroup


-- | Benchmarks are actually just a regular 'Test.Tasty.TestTree' in disguise.
--
-- This is a drop-in replacement for 'Criterion.Benchmark' and 'Gauge.Benchmark'.
--
type Benchmark = TestTree

-- | Run benchmarks and report results, providing
-- an interface compatible with 'Criterion.defaultMain'
-- and 'Gauge.defaultMain'.
--
defaultMain :: [Benchmark] -> IO ()
defaultMain bs = do
    setLocaleEncoding utf8

-- | List of default benchmark ingredients. This is what 'defaultMain' runs.
--
benchIngredients :: [Ingredient]
benchIngredients = [listingTests, composeReporters consoleBenchReporter (composeReporters csvReporter svgReporter)]


-- | 'nf' @f@ @x@ measures time to compute
-- a normal form (by means of 'force') of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentialy @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Here is a textbook antipattern: 'nf' 'sum' @[1..1000000]@.
-- Since an input list is shared by multiple invocations of 'sum',
-- it will be allocated in memory in full, putting immense pressure
-- on garbage collector. Also no list fusion will happen.
-- A better approach is 'nf' (@\\n@ @->@ 'sum' @[1..n]@) @1000000@.
--
-- If you are measuring an inlinable function,
-- it is prudent to ensure that its invocation is fully saturated,
-- otherwise inlining will not happen. That's why one can often
-- see 'nf' (@\\n@ @->@ @f@ @n@) @x@ instead of 'nf' @f@ @x@.
-- Same applies to rewrite rules.
--
-- While @tasty-bench@ is capable to perform micro- and even nanobenchmarks,
-- such measurements are noisy and involve an overhead. Results are more reliable
-- when @f@ @x@ takes at least several milliseconds.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios (imagine benchmarking 'tail'),
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- Drop-in replacement for 'Criterion.nf' and 'Gauge.nf'.
--

-- | 'whnf' @f@ @x@ measures time to compute
-- a weak head normal form of an application of @f@ to @x@.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentialy @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Beware that many educational materials contain examples with 'whnf':
-- this is a wrong default.
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nf' instead.
--
-- Here is a textbook antipattern: 'whnf' ('Data.List.replicate' @1000000@) @1@.
-- This will succeed in a matter of nanoseconds, because weak head
-- normal form forces only the first element of the list.
--
-- Drop-in replacement for 'Criterion.whnf' and 'Gauge.whnf'.
--
whnf :: (a -> b) -> a -> Benchmarkable
whnf = funcToBench id
{-# INLINE whnf #-}

ioToBench :: (b -> c) -> IO b -> Benchmarkable
ioToBench frc act = Benchmarkable go
  where
    go n
      | n == 0    = pure ()
      | otherwise = do
        val <- act
        _ <- evaluate (frc val)
        go (n - 1)
{-# INLINE ioToBench #-}

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its normal form (by means of 'force').
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'nfAppIO' instead.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfIO' ('readFile' @"foo.txt"@).
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for 'Criterion.nfIO' and 'Gauge.nfIO'.
--
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = ioToBench force
{-# INLINE nfIO #-}

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@
-- and compute its weak head normal form.
--
-- Pure subexpression of an effectful computation @x@
-- may be evaluated only once and get cached.
-- To avoid surprising results it is usually preferable
-- to use 'whnfAppIO' instead.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for 'Criterion.whnfIO' and 'Gauge.whnfIO'.
--
whnfIO :: IO a -> Benchmarkable
whnfIO = ioToBench id
{-# INLINE whnfIO #-}

ioFuncToBench :: (b -> c) -> (a -> IO b) -> a -> Benchmarkable
ioFuncToBench frc = (Benchmarkable .) . go
  where
    go f x n
      | n == 0    = pure ()
      | otherwise = do
        val <- f x
        _ <- evaluate (frc val)
        go f x (n - 1)
{-# INLINE ioFuncToBench #-}

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its normal form (by means of 'force').
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentialy @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Note that forcing a normal form requires an additional
-- traverse of the structure. In certain scenarios,
-- especially when 'NFData' instance is badly written,
-- this traversal may take non-negligible time and affect results.
--
-- A typical use case is 'nfAppIO' 'readFile' @"foo.txt"@.
-- However, if your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for 'Criterion.nfAppIO' and 'Gauge.nfAppIO'.
--
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = ioFuncToBench force
{-# INLINE nfAppIO #-}

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of
-- an application of @f@ to @x@.
-- and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- The same thunk of @x@ is shared by multiple calls of @f@. We cannot evaluate
-- @x@ beforehand: there is no 'NFData' @a@ constraint, and potentialy @x@ may
-- be an infinite structure. Thus @x@ will be evaluated in course of the first
-- application of @f@. This noisy measurement is to be discarded soon,
-- but if @x@ is not a primitive data type, consider forcing its evaluation
-- separately, e. g., via 'env' or 'withResource'.
--
-- Computing only a weak head normal form is
-- rarely what intuitively is meant by "evaluation".
-- Unless you understand precisely, what is measured,
-- it is recommended to use 'nfAppIO' instead.
--
-- Lazy I\/O is treacherous.
-- If your goal is not to benchmark I\/O per se,
-- but just read input data from a file, it is cleaner to
-- use 'env' or 'withResource'.
--
-- Drop-in replacement for 'Criterion.whnfAppIO' and 'Gauge.whnfAppIO'.
--
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = ioFuncToBench id
{-# INLINE whnfAppIO #-}

-- | Run benchmarks in the given environment, usually reading large input data from file.
--
-- One might wonder why 'env' is needed,
-- when we can simply read all input data
-- before calling 'defaultMain'. The reason is that large data
-- dangling in the heap causes longer garbage collection
-- and slows down all benchmarks, even those which do not use it at all.
--
-- It is instrumental not only for proper 'IO' actions,
-- but also for a large statically-known data as well. Instead of a top-level
-- definition, which once evaluated will slow down garbage collection
-- during all subsequent benchmarks,
--
-- > largeData :: String
-- > largeData = replicate 1000000 'a'
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ bench "large" $ nf length largeData, ... ]
--
-- use
--
-- > import Control.DeepSeq (force)
-- > import Control.Exception (evaluate)
-- >
-- > main :: IO ()
-- > main = defaultMain
-- >   [ env (evaluate (force (replicate 1000000 'a'))) $ \largeData ->
-- >     bench "large" $ nf length largeData, ... ]
--
-- 'env' is provided only for the sake of compatibility with 'Criterion.env' and 'Gauge.env',
-- and involves 'unsafePerformIO'. Consider using 'withResource' instead.
--
-- 'defaultMain' requires that the hierarchy of benchmarks and their names is
-- independent of underlying 'IO' actions. While executing 'IO' inside 'bench'
-- via 'nfIO' is fine, and reading test data from files via 'env' is also fine,
-- using 'env' to choose benchmarks or their names depending on 'IO' side effects
-- will throw a rather cryptic error message:
--
-- > Unhandled resource. Probably a bug in the runner you're using.
--
env :: NFData env => IO env -> (env -> Benchmark) -> Benchmark
env res = envWithCleanup res (const $ pure ())

-- | Similar to 'env', but includes an additional argument
-- to clean up created environment.
--
-- Provided only for the sake of compatibility
-- with 'Criterion.envWithCleanup' and 'Gauge.envWithCleanup',
-- and involves 'unsafePerformIO'. Consider using 'withResource' instead.
--
envWithCleanup :: NFData env => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
envWithCleanup res fin f = withResource
  (res >>= evaluate . force)
  (void . fin)
  (f . unsafePerformIO)

-- | A path to write results in CSV format, populated by @--csv@.
--
-- This is an option of 'csvReporter' and can be set only globally.
-- Modifying it via 'adjustOption' or 'localOption' does not have any effect.
-- One can however pass it to 'tryIngredients' 'benchIngredients'. For example,
-- here is how to set a default CSV location:
--
-- @
-- import Data.Maybe
-- import System.Exit
-- import Test.Tasty.Bench
-- import Test.Tasty.Ingredients
-- import Test.Tasty.Options
-- import Test.Tasty.Runners
--
-- main :: IO ()
-- main = do
--   let benchmarks = bgroup \"All\" ...
--   opts <- parseOptions benchIngredients benchmarks
--   let opts' = changeOption (Just . fromMaybe (CsvPath "foo.csv")) opts
--   case tryIngredients benchIngredients opts' benchmarks of
--     Nothing -> exitFailure
--     Just mb -> mb >>= \b -> if b then exitSuccess else exitFailure
-- @
--
newtype CsvPath = CsvPath FilePath
  deriving (Typeable)

instance IsOption (Maybe CsvPath) where
  defaultValue = Nothing
  parseValue = Just . Just . CsvPath
  optionName = pure "csv"
  optionHelp = pure "File to write results in CSV format"

-- | Run benchmarks and save results in CSV format.
-- It activates when @--csv@ @FILE@ command line option is specified.
--
csvReporter :: Ingredient
csvReporter = TestReporter [Option (Proxy :: Proxy (Maybe CsvPath))] $
  \opts tree -> do
    CsvPath path <- lookupOption opts
    let names = testsNames opts tree
        namesMap = IM.fromDistinctAscList $ zip [0..] names
    pure $ \smap -> do
      case findNonUniqueElement names of
        Nothing -> pure ()
        Just name -> do -- 'die' is not available before base-4.8
          hPutStrLn stderr $ "CSV report cannot proceed, because name '" ++ name ++ "' corresponds to two or more benchmarks. Please disambiguate them."
          exitFailure
      let augmented = IM.intersectionWith (,) namesMap smap
      bracket
        (do
          h <- openFile path WriteMode
          hSetBuffering h LineBuffering
          hPutStrLn h $ "Name,Mean (ps),2*Stdev (ps)" ++
            (if hasGCStats then ",Allocated,Copied,Peak Memory" else "")
          pure h
        )
        hClose
        (`csvOutput` augmented)
      pure $ const $ isSuccessful smap

findNonUniqueElement :: Ord a => [a] -> Maybe a
findNonUniqueElement = go S.empty
  where
    go _ [] = Nothing
    go acc (x : xs)
      | x `S.member` acc = Just x
      | otherwise = go (S.insert x acc) xs

csvOutput :: Handle -> IntMap (TestName, TVar Status) -> IO ()
csvOutput h = traverse_ $ \(name, tv) -> do
  let csv = if hasGCStats then csvEstimateWithGC else csvEstimate
  r <- atomically $ readTVar tv >>= \s -> case s of Done r -> pure r; _ -> retry
  case safeRead (resultDescription r) of
    Nothing -> pure ()
    Just (Response est _ _) -> do
      msg <- formatMessage $ csv est
      hPutStrLn h (encodeCsv name ++ ',' : msg)

encodeCsv :: String -> String
encodeCsv xs
  | any (`elem` xs) ",\"\n\r"
  = '"' : go xs -- opening quote
  | otherwise = xs
  where
    go [] = '"' : [] -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys) = y : go ys

-- | A path to plot results in SVG format, populated by @--svg@.
--
-- This is an option of 'svgReporter' and can be set only globally.
-- Modifying it via 'adjustOption' or 'localOption' does not have any effect.
-- One can however pass it to 'tryIngredients' 'benchIngredients'.
--


-- | A path to read baseline results in CSV format, populated by @--baseline@.
--
-- This is an option of 'csvReporter' and can be set only globally.
-- Modifying it via 'adjustOption' or 'localOption' does not have any effect.
-- One can however pass it to 'tryIngredients' 'benchIngredients'.
--

-- | A well-formed CSV entry contains an even number of quotes: 0, 2 or more.
joinQuotedFields :: [String] -> [String]
joinQuotedFields [] = []
joinQuotedFields (x : xs)
  | areQuotesBalanced x = x : joinQuotedFields xs
  | otherwise = case span areQuotesBalanced xs of
    (_, [])      -> [] -- malformed CSV
    (ys, z : zs) -> unlines (x : ys ++ [z]) : joinQuotedFields zs
  where
    areQuotesBalanced = even . length . filter (== '"')

estTime :: Estimate -> Double
estTime = word64ToDouble . measTime . estMean

newtype TestName = TestName String

compareVsBaseline :: S.Set String -> TestName -> Estimate -> Int64
compareVsBaseline baseline name (Estimate m stdev) = case mOld of
  Nothing -> 0
  Just (oldTime, oldDoubleSigma)
    -- time and oldTime must be signed integers to use 'abs'
    | abs (time - oldTime) < max (2 * word64ToInt64 stdev) oldDoubleSigma -> 0
    | otherwise -> 100 * (time - oldTime) `quot` oldTime
  where
    time = word64ToInt64 $ measTime m

    mOld :: Maybe (Int64, Int64)
    mOld = do
      let prefix = encodeCsv name ++ ","
      (line, furtherLines) <- S.minView $ snd $ S.split prefix baseline

      case S.minView furtherLines of
        Nothing -> pure ()
        Just (nextLine, _) -> case stripPrefix prefix nextLine of
          Nothing -> pure ()
          -- If there are several lines matching prefix, skip them all.
          -- Should not normally happen, 'csvReporter' prohibits repeating test names.
          Just{}  -> Nothing

      (timeCell, ',' : rest) <- span (/= ',') <$> stripPrefix prefix line
      let doubleSigmaCell = takeWhile (/= ',') rest
      (,) <$> safeRead timeCell <*> safeRead doubleSigmaCell

formatSlowDown :: Int64 -> String
formatSlowDown n = case n `compare` 0 of
  LT -> printf ", %2i%% faster than baseline" (-n)
  EQ -> ""
  GT -> printf ", %2i%% slower than baseline" n





int64ToDouble :: Int64 -> Double
int64ToDouble = fromIntegral

word64ToInt64 :: Word64 -> Int64
word64ToInt64 = fromIntegral

#if !MIN_VERSION_base(4,10,0) && MIN_VERSION_base(4,6,0)
int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral
#endif

-}

word64ToDouble :: Word64 -> Double
word64ToDouble = fromIntegral

predict
  :: Measurement -- ^ time for one run
  -> Measurement -- ^ time for two runs
  -> Estimate
predict (Measurement t1 a1 c1 m1) (Measurement t2 a2 c2 m2) = Estimate
  { estMean  = Measurement t (fit a1 a2) (fit c1 c2) (max m1 m2)
  , estStdev = truncate (sqrt d :: Double)
  }
  where
    fit x1 x2 = x1 `quot` 5 + 2 * (x2 `quot` 5)
    t = fit t1 t2
    sqr x = x * x
    d = sqr (word64ToDouble t1 -     word64ToDouble t)
      + sqr (word64ToDouble t2 - 2 * word64ToDouble t)

nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = funcToBench force
{-# INLINE nf #-}

funcToBench :: (b -> c) -> (a -> b) -> a -> Benchmarkable
funcToBench frc = (Benchmarkable .) . go
  where
    go f x n
      | n == 0    = pure ()
      | otherwise = do
        _ <- evaluate (frc (f x))
        go f x (n - 1)
{-# INLINE funcToBench #-}

-- | Show picoseconds, fitting number in 4 characters.
showPicos4 :: Word64 -> String
showPicos4 i
  | t < 995   = printf "%3.0f  ps"  t
  | t < 995e1 = printf "%4.2f ns"  (t / 1e3)
  | t < 995e2 = printf "%4.1f ns"  (t / 1e3)
  | t < 995e3 = printf "%3.0f  ns" (t / 1e3)
  | t < 995e4 = printf "%4.2f μs"  (t / 1e6)
  | t < 995e5 = printf "%4.1f μs"  (t / 1e6)
  | t < 995e6 = printf "%3.0f  μs" (t / 1e6)
  | t < 995e7 = printf "%4.2f ms"  (t / 1e9)
  | t < 995e8 = printf "%4.1f ms"  (t / 1e9)
  | t < 995e9 = printf "%3.0f  ms" (t / 1e9)
  | otherwise = printf "%4.3f s"   (t / 1e12)
  where
    t = word64ToDouble i
