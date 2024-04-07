module Main where

import qualified Site
import System.Directory (doesPathExist, listDirectory, makeAbsolute, removePathForcibly)
import System.FilePath ((</>))
import Test.HUnit

-- tests building a site from an example markup src directory:
-- test/lumea-root/site/markup
-- ├── dir1
-- │   └── dir2
-- │       └── deep-file.org
-- ├── empty-dir
-- ├── posts
-- │   └── hello-world.org
-- ├── root.org
-- └── stream.org
buildSiteFromPathTest :: IO Test
buildSiteFromPathTest = do
  let lumeaRoot = "test/lumea-root"
  let htmlDir = lumeaRoot </> "site/html"
  Site.buildSite $ Just lumeaRoot

  rootExists <- doesPathExist $ htmlDir </> "root.html"
  streamExists <- doesPathExist $ htmlDir </> "stream.html"
  postsExists <- doesPathExist $ htmlDir </> "posts"
  helloWorldExists <- doesPathExist $ htmlDir </> "posts/hello-world.html"
  dir1Exists <- doesPathExist $ htmlDir </> "dir1"
  dir2Exists <- doesPathExist $ htmlDir </> "dir1" </> "dir2"
  deepFileExists <- doesPathExist $ htmlDir </> "dir1" </> "dir2" </> "deep-file.html"
  emptyDirExists <- doesPathExist $ htmlDir </> "empty-dir"

  return $
    TestLabel "Build site from path" $
      TestList
        [ TestCase (assertBool "root.org exists" rootExists),
          TestCase (assertBool "streame.org exists" rootExists),
          TestCase (assertBool "posts exists" rootExists),
          TestCase (assertBool "posts/hello-world.org exists" rootExists),
          TestCase (assertBool "dir1 exists" dir1Exists),
          TestCase (assertBool "dir1/dir2 exists" dir2Exists),
          TestCase (assertBool "dir1/dir2/deep-file.html exists" deepFileExists),
          TestCase (assertBool "empty-dir does not exist" $ not emptyDirExists)
        ]

buildSiteTest :: IO Test
buildSiteTest = do
  -- clean target dir before testing
  listDirectory "test/lumea-root/site/html" >>= mapM_ removePathForcibly
  -- run tests
  buildSiteFromPathTest

main :: IO ()
main = buildSiteTest >>= runTestTTAndExit
