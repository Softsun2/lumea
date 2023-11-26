module Main where


import Text.Pandoc
import Text.Pandoc.Walk
import qualified Data.Text as T
import System.Directory
  (doesFileExist, doesDirectoryExist, listDirectory, createDirectoryIfMissing)
import qualified System.FilePath as FP
import Control.Monad (forM_)


-- Replaces the file extension of a filepath.
replaceExtension :: T.Text -> T.Text -> FilePath -> FilePath
replaceExtension from to path
  | from `T.isSuffixOf` T.pack path = path FP.-<.> T.unpack to
  | otherwise = error $ path <> " is not of type " <> T.unpack from <> "."


-- Converts a pandoc inline link of file extension `from` to `to`.
convertLink :: T.Text -> T.Text -> Inline -> Inline
convertLink from to (Link attr inline (url, alt))
  | from `T.isSuffixOf` url =
    let convertedPath = replaceExtension from to (T.unpack url) in
    Link attr inline (T.pack convertedPath, alt)
  | otherwise = Link attr inline (url, alt)
convertLink _ _ i = i


-- Converts the contents of a `from` file type to a `to` file type
-- where `src` is the input file and `dest` is the output file.
convertContents :: T.Text -> T.Text -> FilePath -> FilePath -> IO ()
convertContents from to src dest
  | from `T.isSuffixOf` T.pack src && to `T.isSuffixOf` T.pack dest = do
      input <- readFile src
      result <- runIO $ do
        doc <- readOrg def (T.pack input)
        let convertedDoc = walk (convertLink from to) doc
        template <- compileDefaultTemplate to
        writeHtml5String def{ writerTemplate = Just template } convertedDoc
      str <- handleError result
      writeFile dest (T.unpack str)
  | otherwise = error $
    T.unpack from <> " does not match " <> src <> " or " <>
    T.unpack to <> " does not match " <> dest <> "."


-- Applies a function `f` recursively to a file or directory specified
-- by a filepath.
filePathApply :: (FilePath -> IO ()) -> FilePath -> IO ()
filePathApply f fp = do
  isFile <- doesFileExist fp
  isDir <- doesDirectoryExist fp
  if isFile then
    f fp
  else if isDir then do
    f fp
    subFilePaths <- listDirectory fp
    forM_ subFilePaths $ \subFilePath -> do
      filePathApply f $ fp FP.</> subFilePath
  else
    error "Expected file or directory."


-- Returns a path where the `root` of `fp` is swapped with `newRoot`.
swapRoot :: FilePath -> FilePath -> FilePath -> FilePath
swapRoot root newRoot fp
  | T.pack root `T.isPrefixOf` T.pack fp =
    let splitFp = FP.splitPath fp in
    let splitRoot = FP.splitPath root in
    newRoot FP.</>
      (FP.joinPath .
       reverse .
       take (length splitFp - length splitRoot) $ reverse splitFp)
  | otherwise = error $ root <> " is not the root of " <> fp <> "."


-- Entry point.
main :: IO ()
main = do
  let from = T.pack "org"
  let to = T.pack "html"
  let src = "./site/org"
  let dest = "./site/html"

  -- Function passed to `filePathApply` used to convert the contents of
  -- a directory into a directory with the same structure but markup
  -- documents.
  let lumeaApply :: FilePath -> IO ()
      lumeaApply fp = do
        isFile <- doesFileExist fp
        isDir <- doesDirectoryExist fp
        if isFile then
          let swappedRoot = swapRoot src dest fp in
          convertContents from to fp (replaceExtension from to swappedRoot)
        else if isDir then
          -- make mirror dir if it doesn't exist
          let swappedRoot = swapRoot src dest fp in
            createDirectoryIfMissing True swappedRoot
        else error $ "Expected file or directory, got " <> fp <> "."
        
  filePathApply lumeaApply src
    

  -- cwdFilePath <- getCurrentDirectory
  -- filePathApply putStrLn cwdFilePath

  -- before worrying about any details let's just get a "site" converted!
