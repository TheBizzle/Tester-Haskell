module Tester.RunSettings(Settings(..), cellsToSettings) where

import Control.Arrow

import Data.List hiding (insert)
import Data.Set  hiding (foldr)

import Tester.Dialect

data Settings
  = Settings {
    testNums       :: Set Int,
    isStackTracing :: Bool
  } deriving (Eq, Show)

a |> f = f a

cellsToSettings :: (CellBox x) => x -> Settings
cellsToSettings cellBox = foldr constructSettings baseSettings optimizedCells
  where
    baseSettings   = (Settings empty False)
    optimizedCells = cellBox |> (unbox >>> optimize)

constructSettings :: OptCell -> Settings -> Settings
constructSettings OStackTrace (Settings nums _)       = Settings nums    True
constructSettings (ORun num)  (Settings nums tracing) = Settings newNums tracing
  where
    newNums = insert num nums

optimize :: [FlagCell] -> [OptCell]
optimize = sort >>> foldr f []
  where
    f (ToggleCell StackTrace) acc@(OStackTrace : _)     = acc
    f (ToggleCell StackTrace) acc                       = OStackTrace : acc
    f (Run        num)        acc                       = (ORun num)  : acc
    f (DontRun    num)        ((ORun h) : t) | h == num = t
    f (DontRun    _)          acc                       = acc

data OptCell
  = OStackTrace
  | ORun { num :: Int }
