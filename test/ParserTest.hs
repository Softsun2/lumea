module Main where

import           Parser
import           Test.HUnit
import           Data.Either (isLeft)

testPlainText = TestLabel "Parse PlainText"
  $ TestList
    [ TestCase (assertBool "Empty string fail" (isLeft (parse' plainText "")))
    , TestCase
        (assertEqual
           "One char"
           (OfPlainText (PlainText "A"))
           (forceParse plainText "A"))
    , TestCase
        (assertEqual
           "Ascii char"
           (OfPlainText (PlainText "ø"))
           (forceParse plainText "ø"))
    , TestCase
        (assertEqual
           "Hello world"
           (OfPlainText (PlainText "Hello, World!"))
           (forceParse plainText "Hello, World!"))
    , TestCase
        (assertEqual
           "Collapsed whitespace, all whitespace"
           (OfPlainText (PlainText ""))
           (forceParse plainText " \n\t\r "))
    , TestCase
        (assertEqual
           "Collapsed whitespace, sentences"
           (OfPlainText (PlainText "Hi there. How are you?"))
           (forceParse plainText "   Hi  there. \nHow  are you? "))]

testLineBreak = TestLabel "Parse LineBreak"
  $ TestList
    [ TestCase (assertBool "Empty string fail" (isLeft (parse' lineBreak "")))
    , TestCase
        (assertBool "Newline string fail" (isLeft (parse' lineBreak "\n")))
    , TestCase
        (assertBool
           "Three backslashes fail"
           (isLeft (parse' lineBreak "\\\\\\")))
    , TestCase
        (assertEqual
           "Linebreak with no spaces or tabs"
           LineBreak
           (forceParse lineBreak "\\\\"))
    , TestCase
        (assertEqual
           "Linebreak with space"
           LineBreak
           (forceParse lineBreak "\\\\ "))
    , TestCase
        (assertEqual
           "Linebreak with tab"
           LineBreak
           (forceParse lineBreak "\\\\\t"))
    , TestCase
        (assertEqual
           "Linebreak with space and tab"
           LineBreak
           (forceParse lineBreak "\\\\ \t"))
    , TestCase
        (assertEqual
           "Linebreak then newline"
           LineBreak
           (forceParse lineBreak "\\\\\n"))
    , TestCase
        (assertBool
           "Linebreak at beginning of line fail"
           (isLeft (parse' lineBreak "\\\\end")))]

-- todo: can I get the number of test cases to print to be accurate?
-- 1 of 1 test suites (1 of 1 test cases) passed.
main :: IO ()
main = runTestTTAndExit (TestList [testPlainText, testLineBreak])
