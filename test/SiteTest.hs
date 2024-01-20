module Main where

import qualified Site
import Test.HUnit

passedTest :: Test
passedTest = TestLabel "atRoot" $ TestCase (assertEqual "1 equals 1" 1 1)

failedTest :: Test
failedTest = TestLabel "atRoot" $ TestCase (assertEqual "1 equals 1" 1 2)

main :: IO ()
main = runTestTTAndExit $ TestList [passedTest, failedTest]
