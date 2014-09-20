module Main where

import qualified UnitTests

import Test.Framework.Runners.Console (defaultMain)

main = defaultMain $ [UnitTests.tests]
