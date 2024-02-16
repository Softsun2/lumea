{-# LANGUAGE OverloadedStrings #-}

module Meta where

import Data.Text
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk

-- the wip semantic format of the metadata blocks
-- this isn't really a great idea as the blocks can be used interchangably
-- depending on the markup
exampleMetadataBlocks :: Blocks
exampleMetadataBlocks =
  divWith nullAttr $
    header 3 (str "lumea-metadata")
      <> bulletList
        [ plain $ str "key1" <> space <> str "value1",
          plain $ str "key2" <> space <> str "value2"
        ]

-- utility to test for now
readMarkdownFile :: FilePath -> IO Pandoc
readMarkdownFile f = readFile f >>= runIOorExplode . readMarkdown def . Data.Text.pack

readOrgFile :: FilePath -> IO Pandoc
readOrgFile f = readFile f >>= runIOorExplode . readOrg def . Data.Text.pack

toMarkdown :: Pandoc -> IO Text
toMarkdown p = runIOorExplode $ do
  template <- compileDefaultTemplate "html"
  writeMarkdown def {writerTemplate = Just template} p

toOrg :: Pandoc -> IO Text
toOrg p = runIOorExplode $ do
  template <- compileDefaultTemplate "html"
  writeOrg def {writerTemplate = Just template} p

toHtml :: Pandoc -> IO Text
toHtml p = runIOorExplode $ do
  template <- compileDefaultTemplate "html"
  writeHtml5String def {writerTemplate = Just template} p

-- what needs to be implemented
-- filterMetadata :: Pandoc -> Pandoc
-- filterMetadata (Pandoc m blocks) =
-- take head block
-- if head block is the magic header format
-- walk metadata and set each meta data
--    case head blocks of

-- filterSubstituteMetadata :: Pandoc -> Pandoc
-- filterSubstituteMetadata = walk substituteMetadata
--   where
--     substituteMetadata :: Inline -> Inline
--     substituteMetadata i =
--       if contains $*$
--       replace with Metadata[$*$]
