module UnitTests where

import Control.Arrow((>>>))

import Data.Set(empty, fromList, insert, Set, singleton)

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.HUnit((@?=), testCase)

import Tester.Dialect(andAlso, butAlso, CellBox, excludingTo, FlagCell(DontRun, Run), FlagCells(FlagCells), runningTo, ToggleFlag(StackTrace), unbox)
import Tester.RunSettings(cellsToSettings, Settings(Settings))

tests = testGroup "Test settings DSL" [
   testCells "Simple run"                 (Run 2)                                                          (singleton 2)                                      False
 , testCells "Simple range"               (5 `runningTo` 20)                                               (fromList [5..20])                                 False
 , testCells "Simple flag"                (StackTrace)                                                     empty                                              True
 , testCells "Simples combined"           (Run 2 `andAlso` 5 `runningTo` 20 `butAlso` StackTrace)          ((fromList >>> (insert 2)) [5..20])                True
 , testCells "Unnecessary exemption 1"    (DontRun 2)                                                      empty                                              False
 , testCells "Unnecessary exemption 2"    (Run 6 `andAlso` DontRun 2)                                      (singleton 6)                                      False
 , testCells "Unnecessary exempt range 1" (5 `excludingTo` 10)                                             empty                                              False
 , testCells "Unnecessary exempt range 2" (15 `runningTo` 25 `andAlso` 5 `excludingTo` 10)                 (fromList [15..25])                                False
 , testCells "Exemption from single 1"    (Run 2 `andAlso` DontRun 2)                                      empty                                              False
 , testCells "Exemption from single 2"    (Run 101 `andAlso` Run 9 `andAlso` DontRun 9 `andAlso` Run 10)   (fromList [10, 101])                               False
 , testCells "Exemption from range 1"     (1 `runningTo` 10 `butAlso` DontRun 10)                          (fromList [1..9])                                  False
 , testCells "Exemption from range 2"     (1 `runningTo` 10 `butAlso` DontRun 9)                           ((fromList >>> (insert 10)) [1..8])                False
 , testCells "Exempt range from single 1" (Run 6 `butAlso` 1 `excludingTo` 10)                             empty                                              False
 , testCells "Exempt range from single 2" (Run 6 `butAlso` 1 `excludingTo` 10 `andAlso` Run 12)            (singleton 12)                                     False
 , testCells "Exempt range from range 1"  (1 `runningTo` 20 `butAlso` 5 `excludingTo` 10)                  (fromList [1,2,3,4,11,12,13,14,15,16,17,18,19,20]) False
 , testCells "Exempt range from range 2"  (10 `runningTo` 20 `butAlso` 5 `excludingTo` 12)                 (fromList [13..20])                                False
 , testCells "Exempt range from range 3"  (10 `runningTo` 20 `butAlso` 17 `excludingTo` 25)                (fromList [10..16])                                False
 , testCells "Exempt range from mix 1"    (1 `runningTo` 20 `butAlso` 3 `excludingTo` 18 `andAlso` Run 25) (fromList [1,2,19,20,25])                          False
 , testCells "Exempt range from mix 2"    (5 `runningTo` 20 `butAlso` 3 `excludingTo` 18 `andAlso` Run 1)  (fromList [1, 19, 20])                             False
 , testCells "Bringing it all back home"  (Run 22 `andAlso` 10 `runningTo` 15 `butAlso` 11 `excludingTo` 14 `butAlso` Run 20 `andAlso` StackTrace `andAlso` Run 5 `andAlso` DontRun 6 `butAlso` Run 6) (fromList [5, 10, 15, 20, 22]) True
 ]

testCells :: (CellBox b) => String -> b -> Set Int -> Bool -> TestTree
testCells desc box numSet isStackTracing = testCase desc assertion
  where
    cells     = FlagCells $ unbox box
    settings  = Settings numSet isStackTracing
    assertion = cellsToSettings cells @?= settings
