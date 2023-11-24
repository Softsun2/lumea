module Main where

import qualified Text.Pandoc as P
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory, getCurrentDirectory)
import Control.Monad (when, forM_)

filePathApply :: (FilePath -> IO ()) -> FilePath -> IO ()
filePathApply f fp = do
  f fp
  isDir <- doesDirectoryExist fp
  when isDir $ do
    subFilePaths <- listDirectory fp
    forM_ subFilePaths $ \subFilePath -> do
      filePathApply f subFilePath
    
main :: IO ()
main = do
  result <- P.runIO $ do
    doc <- P.readMarkdown P.def (T.pack "[testing](url)")
    P.writeOrg P.def doc
  rst <- P.handleError result
  TIO.putStrLn rst

  cwdFilePath <- getCurrentDirectory
  filePathApply putStrLn cwdFilePath
