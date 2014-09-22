module Tester.Suite(Result, runTests, Suite(..), unsafeRunTests) where

  import Control.Arrow

  import Data.Bifoldable
  import Data.List.NonEmpty as NEL hiding (map)
  import Data.Map                  hiding (map, foldr)
  import Data.Monoid
  import Data.Set           as Set hiding (map, foldr)
  import Data.Validation

  import System.IO.Unsafe

  import Tester.Dialect
  import Tester.RunSettings

  a |> f = f a

  type Result f s = Validation (NonEmpty f) s

  data TestResult
    = TestSuccess
    | TestFailure { msg :: String } deriving (Eq, Show)

  data Suite a b c
    = Suite {
      testMap    :: Map Int a,
      runTest    :: a -> (Result b c),
      failsToStr :: NonEmpty b -> String,
      succToStr  :: c -> String
    }

  instance Monoid TestResult where
    mempty = TestSuccess
    mappend   (TestFailure m1)   (TestFailure m2) = TestFailure $ m1 ++ m2
    mappend x@(TestFailure _)  _                  = x
    mappend _                  x@(TestFailure _)  = x
    mappend _                  _                  = TestSuccess

  runTests :: (Show b) => FlagCells -> (Suite a b c) -> [TestResult]
  runTests = generateResults >>> (fmap resultToTR .)

  unsafeRunTests :: (Show b) => FlagCells -> (Suite a b c) -> [TestResult]
  unsafeRunTests c s@(Suite _ _ failsToStr succToStr) = seq (unsafePerformIO evilIO) testResults
    where
      results     = generateResults c s
      testResults = fmap resultToTR results
      strs        = fmap (bifoldMap failsToStr succToStr) results
      evilIO      = mapM_ putStrLn strs

  generateResults :: (Show b) => FlagCells -> (Suite a b c) -> [Result b c]
  generateResults cells (Suite testMap runTest _ _) =
    cells |> (cellsToSettings >>> testNums >>> Set.toList >>> (fmap ((testMap!) >>> runTest)))

  resultToTR :: (Show a) => Result a b -> TestResult
  resultToTR = bifoldMap failToFailR (\_ -> TestSuccess)
    where
      failToFailR :: (Show f) => NonEmpty f -> TestResult
      failToFailR = (NEL.toList >>> (fmap show) >>> (foldr mappend mempty) >>> TestFailure)
