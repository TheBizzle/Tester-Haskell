module Tester.Dialect(CellBox, FlagCell(..), FlagCells(..), ToggleFlag(..), andAlso, butAlso, excludingTo, notRunning, runningTo) where

  data ToggleFlag
    = Talkative
    | StackTrace deriving (Eq, Show)

  data FlagCell
    = ToggleCell { flag :: ToggleFlag }
    | DontRun    { num :: Int }
    | Run        { num :: Int } deriving (Eq, Show)

  data FlagCells
    = FlagCells {
        cells :: [FlagCell]
      } deriving (Eq, Show)

  notRunning :: Int -> FlagCell
  notRunning x = DontRun x

  runningTo :: Int -> Int -> FlagCells
  runningTo x y = FlagCells (fmap (\z -> Run z) [x..y])

  excludingTo :: Int -> Int -> FlagCells
  excludingTo x y = FlagCells (fmap (\z -> DontRun z) [x..y])

  andAlso, butAlso :: (CellBox a, CellBox b) => a -> b -> FlagCells
  andAlso x y = FlagCells ((unbox x) ++ (unbox y))
  butAlso = andAlso

  class CellBox x where
    unbox :: x -> [FlagCell]

  instance CellBox FlagCells where
    unbox (FlagCells cells) = cells

  instance CellBox FlagCell where
    unbox cell = [cell]

  instance CellBox ToggleFlag where
    unbox flag = unbox (ToggleCell flag)

  infixl 6 `runningTo`,`excludingTo`
  infixl 2 `andAlso`,`butAlso`
