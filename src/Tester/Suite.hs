module Tester.Suite(Result, runTests, Suite(..), TestResult(..)) where

import Data.Bifoldable(bifoldMap)
import Data.List.NonEmpty(NonEmpty)
import Data.Map((!))
import Data.Validation(Validation)

import qualified Data.Set as Set

import Tester.Dialect(FlagCells)
import Tester.RunSettings(cellsToSettings, testNums)

type Result f s = Validation (NonEmpty f) s

data TestResult
  = TestSuccess
  | TestFailure { msg :: Text } deriving (Eq, Show)

data Suite a b c
  = Suite {
    testMap    :: Map Int a,
    runTest    :: a -> Result b c,
    failsToStr :: NonEmpty b -> Text,
    succToStr  :: c -> Text
  }

instance Semigroup TestResult where
  (  TestFailure m1) <>   (TestFailure m2) = TestFailure $ m1 <> m2
  x@(TestFailure _)  <> _                  = x
  _                  <> x@(TestFailure _)  = x
  _                  <> _                  = TestSuccess

instance Monoid TestResult where
  mempty  = TestSuccess
  mappend = (<>)

runTests :: FlagCells -> (Suite a b c) -> [TestResult]
runTests c s@(Suite _ _ failsToStr _) = fmap (resultToTR failsToStr) $ generateResults c s

generateResults :: FlagCells -> (Suite a b c) -> [Result b c]
generateResults cells (Suite testMap runTest _ _) =
  cells |> (cellsToSettings >>> testNums >>> Set.toList >>> (fmap $ (testMap !) >>> runTest))

resultToTR :: (NonEmpty a -> Text)-> Result a b -> TestResult
resultToTR fToStr = bifoldMap (fToStr >>> TestFailure) $ const TestSuccess
