module Main where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory, getCurrentDirectory)
import Control.Monad (when)

dirApply :: [FilePath]
  -> (FilePath -> IO ())
  -> IO ()
dirApply [] _ = return ()
dirApply (fp:fps) f = do
  f fp
  isDir <- doesDirectoryExist fp
  when isDir $ do
    subFilePaths <- listDirectory fp
    dirApply subFilePaths f
  dirApply fps f
      
main :: IO ()
main = do
  result <- P.runIO $ do
    doc <- P.readMarkdown P.def (T.pack "[testing](url)")
    P.writeOrg P.def doc
  rst <- P.handleError result
  TIO.putStrLn rst

  cwdFilePath <- getCurrentDirectory
  cwdFilePaths <- listDirectory cwdFilePath
  dirApply cwdFilePaths putStrLn
