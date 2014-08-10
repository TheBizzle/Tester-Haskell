module Tester.TestRunSettings(Settings, cellsToSettings) where

  import Control.Arrow

  import Data.List hiding (insert)
  import Data.Set  hiding (foldr)

  import Tester.Dialect

  data Settings
    = Settings {
      testNums       :: Set Int,
      isTalkative    :: Bool,
      isStackTracing :: Bool
    }

  cellsToSettings :: FlagCells -> Settings
  cellsToSettings (FlagCells cells) = foldr constructSettings baseSettings optimizedCells
    where
      baseSettings   = (Settings empty False False)
      optimizedCells = optimize cells

  constructSettings :: OptCell -> Settings -> Settings
  constructSettings OTalkative  (Settings nums _       tracing) = Settings nums    True    tracing
  constructSettings OStackTrace (Settings nums talking _)       = Settings nums    talking True
  constructSettings (ORun num)  (Settings nums talking tracing) = Settings newNums talking tracing
    where
      newNums = insert num nums

  optimize :: [FlagCell] -> [OptCell]
  optimize = sort >>> foldr f []
    where
      f (ToggleCell Talkative)  acc@(OTalkative : _)      = acc
      f (ToggleCell Talkative)  acc                       = OTalkative  : acc
      f (ToggleCell StackTrace) acc@(OStackTrace : _)     = acc
      f (ToggleCell StackTrace) acc                       = OStackTrace : acc
      f (Run        num)        acc                       = (ORun num)  : acc
      f (DontRun    num)        ((ORun h) : t) | h == num = t
      f (DontRun    _)          acc                       = acc

  data OptCell
    = OTalkative
    | OStackTrace
    | ORun { num :: Int }
