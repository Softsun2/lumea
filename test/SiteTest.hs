module Main where

import           Control.Monad.Reader (runReaderT)
import           Data.Time (getCurrentTime)
import qualified Site
import           System.Directory (doesPathExist, listDirectory, makeAbsolute
                                 , removePathForcibly, setModificationTime)
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

isDirtyTest :: IO Test
isDirtyTest = do
  -- clean target dir before testing
  let htmlDir = "test/site-test/site/html"
  listDirectory htmlDir >>= mapM_ (removePathForcibly . (htmlDir </>))
  let lumeaRoot = "test/site-test"
  let getDirty _ = runReaderT
        (Site.isDirty $ lumeaRoot </> "site/markup/root.org")
        (Just lumeaRoot)
  beforeBuild <- getDirty ()
  Site.buildSite $ Just lumeaRoot -- assume build is working
  afterBuild <- getDirty ()
  getCurrentTime >>= setModificationTime (lumeaRoot </> "site/markup/root.org")
  afterBuildAndTouch <- getDirty ()
  return
    $ TestLabel "Dirty file checks"
    $ TestList
      [ TestCase (assertBool "Dirty before build" beforeBuild)
      , TestCase (assertBool "Not dirty after build" $ not afterBuild)
      , TestCase
          (assertBool
             "Dirty after build and touching source"
             afterBuildAndTouch)]

main :: IO ()
main = do
  buildSiteTest' <- buildSiteTest
  isDirtyTest' <- isDirtyTest
  runTestTTAndExit
    $ TestLabel "Site tests"
    $ TestList [buildSiteTest', isDirtyTest']
