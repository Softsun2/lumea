module Site where

import Data.Text as T
import System.Directory (findFile, getCurrentDirectory, makeAbsolute)
import System.FilePath (isExtensionOf, makeRelative, replaceExtension, takeDirectory, takeExtension, (-<.>), (</>))
import Text.Pandoc
import Text.Pandoc.Walk (Walkable (walk))

getLumeaRoot :: Maybe FilePath -> IO FilePath
getLumeaRoot Nothing = getCurrentDirectory
getLumeaRoot (Just userPath) = return userPath

getLumeaMarkupPath :: IO FilePath
getLumeaMarkupPath =
  getLumeaRoot Nothing >>= (\root -> return $ root </> "site/markup")

getLumeaHtmlPath :: IO FilePath
getLumeaHtmlPath =
  getLumeaRoot Nothing >>= (\root -> return $ root </> "site/html")

isDirty :: FilePath -> Bool
isDirty _ = True

replaceLink :: Inline -> Inline
replaceLink (Link attr inline (url, alt))
  | "org" `isExtensionOf` T.unpack url =
      let convertedPath = T.unpack url -<.> "html"
       in Link attr inline (T.pack convertedPath, alt)
  | otherwise = Link attr inline (url, alt)
replaceLink i = i

replaceLinks :: Pandoc -> Pandoc
replaceLinks = walk replaceLink

toHtml :: T.Text -> IO T.Text
toHtml markupContents = runIOorExplode $ do
  template <- compileDefaultTemplate (T.pack "html")
  orgPandoc <- readOrg def markupContents
  writeHtml5String
    def {writerTemplate = Just template}
    (replaceLinks orgPandoc)

getMirrorPath :: FilePath -> IO FilePath
getMirrorPath markupPath = do
  lumeaMarkupPath <- getLumeaMarkupPath
  lumeaHtmlPath <- getLumeaHtmlPath
  absoluteMarkupPath <- makeAbsolute markupPath
  maybePath <- findFile [lumeaMarkupPath] absoluteMarkupPath
  case maybePath of
    Just absPath ->
      return $
        lumeaHtmlPath </> makeRelative lumeaMarkupPath absPath -<.> "html"
    Nothing -> error $ "Error: " <> markupPath <> " does not exist."

buildFile :: FilePath -> IO ()
buildFile markupSrc
  | isDirty markupSrc = do
      markupContents <- readFile markupSrc
      htmlContents <- toHtml $ T.pack markupContents
      dest <- getMirrorPath markupSrc
      writeFile dest $ T.unpack htmlContents
  | otherwise = return ()
