module Tester.Suite(Result, runTests, Suite(..), TestResult(..), unsafeRunTests) where

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
      runTest    :: a -> Result b c,
      failsToStr :: NonEmpty b -> String,
      succToStr  :: c -> String
    }

  instance Monoid TestResult where
    mempty = TestSuccess
    mappend   (TestFailure m1)   (TestFailure m2) = TestFailure $ m1 `mappend` m2
    mappend x@(TestFailure _)  _                  = x
    mappend _                  x@(TestFailure _)  = x
    mappend _                  _                  = mempty

  runTests :: FlagCells -> (Suite a b c) -> [TestResult]
  runTests c s@(Suite _ _ failsToStr _) = fmap (resultToTR failsToStr) (generateResults c s)

  unsafeRunTests :: FlagCells -> (Suite a b c) -> [TestResult]
  unsafeRunTests c s@(Suite _ _ failsToStr succToStr) = seq (unsafePerformIO evilIO) testResults
    where
      results     = generateResults c s
      testResults = fmap (resultToTR failsToStr) results
      strs        = fmap (bifoldMap failsToStr succToStr) results
      evilIO      = mapM_ putStrLn strs

  generateResults :: FlagCells -> (Suite a b c) -> [Result b c]
  generateResults cells (Suite testMap runTest _ _) =
    cells |> (cellsToSettings >>> testNums >>> Set.toList >>> (fmap ((testMap!) >>> runTest)))

  resultToTR :: (NonEmpty a -> String)-> Result a b -> TestResult
  resultToTR fToStr = bifoldMap (\nel -> TestFailure $ fToStr nel) (const TestSuccess)
