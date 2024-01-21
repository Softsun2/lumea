module Site (buildSite) where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text as T
import System.Directory (doesFileExist, findFile, getCurrentDirectory, listDirectory, makeAbsolute)
import System.FilePath (isExtensionOf, makeRelative, (-<.>), (</>))
import Text.Pandoc hiding (ReaderT)
import Text.Pandoc.Walk (Walkable (walk))

getLumeaRoot :: ReaderT (Maybe FilePath) IO FilePath
getLumeaRoot = fromMaybe <$> liftIO getCurrentDirectory <*> ask

getLumeaMarkupPath :: ReaderT (Maybe FilePath) IO FilePath
getLumeaMarkupPath = (</> "site/markup") <$> getLumeaRoot

getLumeaHtmlPath :: ReaderT (Maybe FilePath) IO FilePath
getLumeaHtmlPath = (</> "site/html") <$> getLumeaRoot

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

getMirrorPath :: FilePath -> ReaderT (Maybe FilePath) IO FilePath
getMirrorPath markupPath = do
  lumeaMarkupPath <- getLumeaMarkupPath
  lumeaHtmlPath <- getLumeaHtmlPath
  absoluteMarkupPath <- liftIO $ makeAbsolute markupPath
  maybePath <- liftIO $ findFile [lumeaMarkupPath] absoluteMarkupPath
  case maybePath of
    Just absPath ->
      return $
        lumeaHtmlPath </> makeRelative lumeaMarkupPath absPath -<.> "html"
    Nothing -> error $ "Error: " <> markupPath <> " does not exist."

buildFile :: FilePath -> ReaderT (Maybe FilePath) IO ()
buildFile markupSrc
  | isDirty markupSrc = do
      markupContents <- liftIO $ readFile markupSrc
      htmlContents <- liftIO $ toHtml $ T.pack markupContents
      dest <- getMirrorPath markupSrc
      liftIO $ writeFile dest $ T.unpack htmlContents
  | otherwise = return ()
    
buildDir :: FilePath -> ReaderT (Maybe FilePath) IO ()
buildDir dir = do
  entries <- liftIO $ listDirectory dir
  mapM_ buildEntry entries
  where
    buildEntry :: FilePath -> ReaderT (Maybe FilePath) IO ()
    buildEntry entry = do
      let fp = dir </> entry
      entryIsFile <- liftIO $ doesFileExist fp
      if entryIsFile
        then buildFile fp
        else buildDir fp

buildSite :: Maybe FilePath -> IO ()
buildSite userPath = do
    markupPath <- runReaderT getLumeaMarkupPath userPath
    runReaderT (buildDir markupPath) userPath
