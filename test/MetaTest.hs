{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Meta
import Test.HUnit
import Text.Parsec
import Text.Parsec.String (Parser)

-- parseTest is already defined
parseTest' :: (Eq a) => a -> Parser a -> String -> Bool
parseTest' expected parser input =
  case parse parser "" input of
    Left _ -> False
    Right actual -> expected == actual

noSubstitutesTest :: Test
noSubstitutesTest =
  TestLabel "No substitutes" $
    TestList
      [ TestCase
          ( assertBool
              "empty input"
              (parseTest' [] Meta.substitutes "")
          ),
        TestCase
          ( assertBool
              "no dollar signs"
              (parseTest' [] Meta.substitutes "this is my content")
          ),
        TestCase
          ( assertBool
              "just a dollar sign"
              (parseTest' [] Meta.substitutes "$")
          ),
        TestCase
          ( assertBool
              "one dollar sign"
              (parseTest' [] Meta.substitutes "this content has a $ character")
          ),
        TestCase
          ( assertBool
              "zero length key"
              (parseTest' [] Meta.substitutes "a key $$ must not be empty")
          ),
        TestCase
          ( assertBool
              "many zero length keys"
              (parseTest' [] Meta.substitutes "$$$$$$that's a lot of money!")
          ),
        TestCase
          ( assertBool
              "one pair of dollar signs, space within pair"
              (parseTest' [] Meta.substitutes "keys can't $have $ spaces")
          ),
        TestCase
          ( assertBool
              "multiple pairs of dollar signs, space within pairs"
              (parseTest' [] Meta.substitutes "$keys $can't $have $ spaces$")
          )
      ]

oneSubstituteTest :: Test
oneSubstituteTest =
  TestLabel "One substitute" $
    TestList
      [ TestCase
          ( assertBool
              "test1"
              (parseTest' ["key"] Meta.substitutes "$key$")
          ),
        TestCase
          ( assertBool
              "test2"
              (parseTest' ["word2"] Meta.substitutes "word1 $word2$ word3")
          ),
        TestCase
          ( assertBool
              "test3"
              (parseTest' ["start-key"] Meta.substitutes "$start-key$ text")
          ),
        TestCase
          ( assertBool
              "test4"
              (parseTest' ["end-key"] Meta.substitutes "pretext $end-key$")
          ),
        TestCase
          ( assertBool
              "test6"
              (parseTest' ["title"] Meta.substitutes "$title$ money $ talk")
          ),
        TestCase
          ( assertBool
              "test7"
              (parseTest' ["key"] Meta.substitutes "$$ $$$ $$$key$$$ $$$$")
          )
      ]

manySubstitutesTest :: Test
manySubstitutesTest =
  TestLabel "Many substitutes" $
    TestList
      [ TestCase
          ( assertBool
              "test1"
              ( parseTest'
                  [ "title", "date" ]
                  Meta.substitutes
                  "$title$\n$date$\nA sentence. One dollar ($1.00)."
              )
          ),
        TestCase
          ( assertBool
              "test2"
              ( parseTest'
                  ["title", "date", "author"]
                  Meta.substitutes
                  "$title$\n$date$$$author $author$$\nSome$ horribleness."
              )
          )
      ]

substitutesTest :: Test
substitutesTest =
  TestLabel "Substitutes test" $
    TestList [ noSubstitutesTest, oneSubstituteTest, manySubstitutesTest ]

main :: IO ()
main = runTestTTAndExit substitutesTest
