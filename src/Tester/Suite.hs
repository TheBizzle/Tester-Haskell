module Tester.Suite(Result, runTests, Suite(..)) where

  import Control.Arrow

  import Data.Bifoldable
  import Data.List.NonEmpty hiding (toList)
  import Data.Map           hiding (toList)
  import Data.Set
  import Data.Validation

  import Tester.Dialect
  import Tester.RunSettings

  type Result f s = Validation (NonEmpty f) s

  data Suite a b c
    = Suite {
      testMap    :: Map Int a,
      runTest    :: a -> (Result b c),
      failsToStr :: NonEmpty b -> String,
      succToStr  :: c -> String
    }

  runTests :: FlagCells -> (Suite a b c) -> IO ()
  runTests cells (Suite testMap runTest failsToStr succToStr) = mapM_ putStrLn strs
    where
      (Settings testNumSet isStackTracing) = cellsToSettings cells
      numsToRun = toList testNumSet
      results   = fmap ((testMap!) >>> runTest) numsToRun
      strs      = fmap (bifoldMap failsToStr succToStr) results
