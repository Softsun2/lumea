module Site (buildSite) where

import           Control.Monad.Reader
import           Data.Maybe (fromMaybe)
import           Data.Text as T
import           System.Directory (createDirectoryIfMissing, doesFileExist
                                 , doesPathExist, findFile, getCurrentDirectory
                                 , listDirectory, makeAbsolute)
import           System.FilePath (isExtensionOf, makeRelative, takeDirectory
                                , (-<.>), (</>), normalise)
import           Text.Pandoc hiding (ReaderT)
import           Text.Pandoc.Walk (Walkable(walk))

getLumeaRoot :: Maybe FilePath -> IO FilePath
getLumeaRoot = maybe getCurrentDirectory makeAbsolute

getLumeaMarkupPath :: FilePath -> FilePath
getLumeaMarkupPath lumeaRoot = lumeaRoot </> "site/markup"

getLumeaHtmlPath :: FilePath -> FilePath
getLumeaHtmlPath lumeaRoot = lumeaRoot </> "site/html"

-- there must be a way to do this without comparing html file to markup file dates
isDirty :: FilePath -> Bool
isDirty _ = True

replaceLink :: Inline -> Inline
replaceLink link@(Link attr inline (url, alt))
  | "org" `isExtensionOf` T.unpack url =
    Link attr inline ((T.pack . (-<.> "html") . T.unpack) url, alt)
  | otherwise = link
replaceLink i = i

replaceLinks :: Pandoc -> Pandoc
replaceLinks = walk replaceLink

toHtml :: T.Text -> IO T.Text
toHtml markupContents = runIOorExplode
  $ do
    template <- compileDefaultTemplate (T.pack "html")
    orgPandoc <- readOrg def markupContents
    writeHtml5String
      def { writerTemplate = Just template }
      (replaceLinks orgPandoc)

-- todo document me
getMirrorPath :: FilePath -> FilePath -> Maybe FilePath
getMirrorPath lumeaRoot markupPath =
  let relativePath = makeRelative (getLumeaMarkupPath lumeaRoot) markupPath
  in if relativePath == markupPath
     then Nothing
     else Just (getLumeaHtmlPath lumeaRoot </> relativePath -<.> "html")

-- IO
buildFile :: FilePath -> ReaderT (Maybe FilePath) IO ()
buildFile markupSrc
  | isDirty markupSrc = do
    markupContents <- liftIO $ readFile markupSrc
    htmlContents <- liftIO $ toHtml $ T.pack markupContents
    -- make lumeaRoot and markupSrc absolute
    lumeaRoot <- ask >>= liftIO . (getLumeaRoot >=> makeAbsolute)
    liftIO $ createDirectoryIfMissing True (takeDirectory "")
    liftIO $ writeFile "" $ T.unpack htmlContents
  | otherwise = return ()

-- IO
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

-- IO
buildSite :: Maybe FilePath -> IO ()
buildSite userPath = do
  -- markupPath <- getLumeaMarkupPath userPath
  runReaderT (buildDir "") userPath
