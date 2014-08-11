module Tester.Suite(executeSuite, Suite, Result) where

  import Control.Arrow

  import Data.Bifoldable
  import Data.List.NonEmpty hiding (toList)
  import Data.Map           hiding (toList)
  import Data.Set
  import Data.Validation

  type Result f s = Validation (NonEmpty f) s

  data Suite a b c
    = Suite {
      testMap    :: Map Int a,
      runTest    :: a -> (Result b c),
      failsToStr :: NonEmpty b -> String,
      succToStr  :: c -> String
    }

  executeSuite :: (Suite a b c) -> (Set Int) -> [String]
  executeSuite (Suite testMap runTest failsToStr succToStr) numSet = strs
    where
      numsToRun = toList numSet
      results   = fmap ((testMap!) >>> runTest) numsToRun
      strs      = fmap (bifoldMap failsToStr succToStr) results
