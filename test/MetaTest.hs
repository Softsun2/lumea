{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Meta
import           Test.HUnit
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Data.Text as T

-- prints the pandoc before and after round trip
roundTrip :: Pandoc -> IO ()
roundTrip p = print p
  *> (runIOorExplode
        (do
           template <- compileDefaultTemplate "markdown"
           writeMarkdown def { writerTemplate = Just template } p)
      >>= runIOorExplode . readMarkdown def { readerStandalone = True }
      >>= print)

printMarkdown :: Pandoc -> IO ()
printMarkdown p = runIOorExplode
  (do
     template <- compileDefaultTemplate "markdown"
     writeMarkdown def { writerTemplate = Just template } p)
  >>= print

exampleLumeaMarkdown :: Pandoc
exampleLumeaMarkdown =
  doc $ para $ codeWith nullAttr "int main() {\nreturn 0;\n}"

exampleLumeaMarkdown' :: Pandoc
exampleLumeaMarkdown' = doc
  $ codeBlock "int main() {\nreturn 0;\n}"
  <> header 1 (str "Title")
  <> para "My content"

-- parseTest is already defined
parseTest' :: (Eq a) => a -> Parser a -> String -> Bool
parseTest' expected parser input = case parse parser "" input of
  Left _       -> False
  Right actual -> expected == actual

noKeysTest :: Test
noKeysTest = TestLabel "No parseKeys"
  $ TestList
    [ TestCase (assertBool "empty input" (parseTest' [] Meta.parseKeys ""))
    , TestCase
        (assertBool
           "no dollar signs"
           (parseTest' [] Meta.parseKeys "this is my content"))
    , TestCase
        (assertBool "just a dollar sign" (parseTest' [] Meta.parseKeys "$"))
    , TestCase
        (assertBool
           "one dollar sign"
           (parseTest' [] Meta.parseKeys "this content has a $ character"))
    , TestCase
        (assertBool
           "zero length key"
           (parseTest' [] Meta.parseKeys "a key $$ must not be empty"))
    , TestCase
        (assertBool
           "many zero length keys"
           (parseTest' [] Meta.parseKeys "$$$$$$that's a lot of money!"))
    , TestCase
        (assertBool
           "one pair of dollar signs, space within pair"
           (parseTest' [] Meta.parseKeys "keys can't $have $ spaces"))
    , TestCase
        (assertBool
           "multiple pairs of dollar signs, space within pairs"
           (parseTest' [] Meta.parseKeys "$keys $can't $have $ spaces$"))]

oneKeyTest :: Test
oneKeyTest = TestLabel "One substitute"
  $ TestList
    [ TestCase (assertBool "test1" (parseTest' ["key"] Meta.parseKeys "$key$"))
    , TestCase
        (assertBool
           "test2"
           (parseTest' ["word2"] Meta.parseKeys "word1 $word2$ word3"))
    , TestCase
        (assertBool
           "test3"
           (parseTest' ["start-key"] Meta.parseKeys "$start-key$ text"))
    , TestCase
        (assertBool
           "test4"
           (parseTest' ["end-key"] Meta.parseKeys "pretext $end-key$"))
    , TestCase
        (assertBool
           "test6"
           (parseTest' ["title"] Meta.parseKeys "$title$ money $ talk"))
    , TestCase
        (assertBool
           "test7"
           (parseTest' ["key"] Meta.parseKeys "$$ $$$ $$$key$$$ $$$$"))]

manyKeysTest :: Test
manyKeysTest = TestLabel "Many parseKeys"
  $ TestList
    [ TestCase
        (assertBool
           "test1"
           (parseTest'
              ["title", "date"]
              Meta.parseKeys
              "$title$\n$date$\nA sentence. One dollar ($1.00)."))
    , TestCase
        (assertBool
           "test2"
           (parseTest'
              ["title", "date", "author"]
              Meta.parseKeys
              "$title$\n$date$$$author $author$$\nSome$ horribleness."))]

parseKeysTest :: Test
parseKeysTest =
  TestLabel "parseKeys test" $ TestList [noKeysTest, oneKeyTest, manyKeysTest]

insertMetadataTest :: IO Test
insertMetadataTest = do
  expected <- readFile "test/meta-test/markdown-expected.md"
  actual <- readFile "test/meta-test/markdown-src.md"
    >>= runIOorExplode . readMarkdown def . T.pack
    >>= runIOorExplode
    . (T.unpack <$>)
    . writeMarkdown def
    . Meta.insertMetadata
  return
    $ TestCase
      (assertEqual
         "Insert metadata from: test/meta-test/markdown-expected.md"
         expected
         actual)

main :: IO ()
main = insertMetadataTest >>= runTestTTAndExit
