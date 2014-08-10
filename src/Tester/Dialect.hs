module Tester.Dialect(CellBox, FlagCell(..), FlagCells(..), ToggleFlag(..), andAlso, butAlso, excludingTo, notRunning, runningTo) where

  data ToggleFlag
    = StackTrace deriving (Eq, Show)

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

  -- Example: [DontRun 0, Run 1, DontRun 2, Run 2, Run 3, DontRun 5, StackTrace]
  instance Ord FlagCell where
    (Run        a)         <= (Run        b)          = a <= b
    (DontRun    a)         <= (DontRun    b)          = a <= b
    (ToggleCell _)         <= (ToggleCell _)          = True
    (ToggleCell _)         <= (DontRun    _)          = False
    (ToggleCell _)         <= (Run        _)          = False
    (DontRun    _)         <= (ToggleCell _)          = True
    (Run        _)         <= (ToggleCell _)          = True
    (Run        a)         <= (DontRun    b)          = a < b
    (DontRun    a)         <= (Run        b)          = a <= b

  infixl 6 `runningTo`,`excludingTo`
  infixl 2 `andAlso`,`butAlso`
