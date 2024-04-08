module Site (buildSite, isDirty) where

import           Control.Monad.Reader
import           Data.Maybe (fromMaybe)
import           Data.Text as T
import           System.Directory (createDirectoryIfMissing, doesFileExist
                                 , doesPathExist, findFile, getCurrentDirectory
                                 , listDirectory, makeAbsolute
                                 , getModificationTime)
import           System.FilePath (isExtensionOf, makeRelative, takeDirectory
                                , (-<.>), (</>))
import           Text.Pandoc hiding (ReaderT, getModificationTime)
import           Text.Pandoc.Walk (Walkable(walk))
import           Data.Time (UTCTime)
import           Data.Time.ISO8601
import           Control.Exception (try)
import           Data.Either (fromRight)

getLumeaRoot :: ReaderT (Maybe FilePath) IO FilePath
getLumeaRoot = do
  userPath <- ask
  case userPath of
    Just userPath' -> liftIO $ makeAbsolute userPath'
    Nothing        -> liftIO getCurrentDirectory

getLumeaMarkupPath :: ReaderT (Maybe FilePath) IO FilePath
getLumeaMarkupPath = (</> "site/markup") <$> getLumeaRoot

getLumeaHtmlPath :: ReaderT (Maybe FilePath) IO FilePath
getLumeaHtmlPath = (</> "site/html") <$> getLumeaRoot

isDirty :: FilePath -> ReaderT (Maybe FilePath) IO Bool
isDirty filepath = do
  markupTime <- liftIO
    $ (try :: IO UTCTime -> IO (Either IOError UTCTime))
    $ getModificationTime filepath
  htmlTime <- getMirrorPath filepath
    >>= (liftIO
         . (try :: IO UTCTime -> IO (Either IOError UTCTime))
         . getModificationTime)
  return $ fromRight True ((>) <$> markupTime <*> htmlTime)

replaceLink :: Inline -> Inline
replaceLink (Link attr inline (url, alt))
  | "org" `isExtensionOf` T.unpack url =
    let convertedPath = T.unpack url -<.> "html"
    in Link attr inline (T.pack convertedPath, alt)
  | otherwise = Link attr inline (url, alt)
replaceLink i = i

toHtml :: T.Text -> IO T.Text
toHtml markupContents = runIOorExplode
  $ do
    template <- compileDefaultTemplate (T.pack "html")
    orgPandoc <- readOrg def markupContents
    writeHtml5String
      def { writerTemplate = Just template }
      (walk replaceLink orgPandoc)

getMirrorPath :: FilePath -> ReaderT (Maybe FilePath) IO FilePath
getMirrorPath markupPath = do
  lumeaMarkupPath <- getLumeaMarkupPath
  lumeaHtmlPath <- getLumeaHtmlPath
  absoluteMarkupPath <- liftIO $ makeAbsolute markupPath
  maybePath <- liftIO $ findFile [lumeaMarkupPath] absoluteMarkupPath
  case maybePath of
    Just absPath -> return
      $ lumeaHtmlPath </> makeRelative lumeaMarkupPath absPath -<.> "html"
    Nothing      -> error $ "Error: " <> markupPath <> " does not exist."

buildFile :: FilePath -> ReaderT (Maybe FilePath) IO ()
buildFile markupSrc
  -- | isDirty markupSrc      -- todo: fix me
   = do
    markupContents <- liftIO $ readFile markupSrc
    htmlContents <- liftIO $ toHtml $ T.pack markupContents
    dest <- getMirrorPath markupSrc
    liftIO $ createDirectoryIfMissing True (takeDirectory dest)
    liftIO $ writeFile dest $ T.unpack htmlContents

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
