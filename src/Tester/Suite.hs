module Tester.Suite(executeSuite, Suite, Result) where

  import Control.Arrow

  import Data.List.NonEmpty hiding (toList)
  import Data.Map           hiding (toList)
  import Data.Set
  import Data.Validation

  type Result f s = Validation (NonEmpty f) s

  data Suite a b c
    = Suite {
      testMap :: Map Int a,
      runTest :: a -> (Result b c)
    }

  executeSuite :: (Suite a b c) -> (Set Int) -> [(Result b c)]
  executeSuite (Suite testMap runTest) numSet = fmap ((testMap!) >>> runTest) numsToRun
    where
      numsToRun = toList numSet
