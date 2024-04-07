module Main where

import qualified Site
import           System.Directory (doesPathExist, listDirectory, makeAbsolute
                                 , removePathForcibly)
import           System.FilePath ((</>))
import           Test.HUnit

-- tests building a site from an example markup src directory:
-- test/site-test/site/markup
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
  let lumeaRoot = "test/site-test"
  let htmlDir = lumeaRoot </> "site/html"
  Site.buildSite $ Just lumeaRoot
  rootExists <- doesPathExist $ htmlDir </> "root.html"
  streamExists <- doesPathExist $ htmlDir </> "stream.html"
  postsExists <- doesPathExist $ htmlDir </> "posts"
  helloWorldExists <- doesPathExist $ htmlDir </> "posts/hello-world.html"
  dir1Exists <- doesPathExist $ htmlDir </> "dir1"
  dir2Exists <- doesPathExist $ htmlDir </> "dir1" </> "dir2"
  deepFileExists
    <- doesPathExist $ htmlDir </> "dir1" </> "dir2" </> "deep-file.html"
  emptyDirExists <- doesPathExist $ htmlDir </> "empty-dir"
  return
    $ TestLabel "Build site from path"
    $ TestList
      [ TestCase (assertBool "root.html exists" rootExists)
      , TestCase (assertBool "stream.html exists" streamExists)
      , TestCase (assertBool "posts exists" postsExists)
      , TestCase (assertBool "posts/hello-world.html exists" helloWorldExists)
      , TestCase (assertBool "dir1 exists" dir1Exists)
      , TestCase (assertBool "dir1/dir2 exists" dir2Exists)
      , TestCase (assertBool "dir1/dir2/deep-file.html exists" deepFileExists)
      , TestCase (assertBool "empty-dir does not exist" $ not emptyDirExists)]

buildSiteTest :: IO Test
buildSiteTest = do
  -- clean target dir before testing
  let htmlDir = "test/site-test/site/html"
  listDirectory htmlDir >>= mapM_ (removePathForcibly . (htmlDir </>))
  -- run tests
  buildSiteFromPathTest

main :: IO ()
main = do
  buildSiteTest >>= runTestTTAndExit
