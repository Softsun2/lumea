{-# LANGUAGE OverloadedStrings #-}

module Meta where

import Data.Sequence
import Data.Text
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk

-- Design:
-- I don't like mixing languages e.g. yaml header with markup body. I don't
-- like seperating declarations from their references e.g. a meta-lockfile
-- referenced from source files.
--
-- What if we could create a strict semantic structure that is compliant with
-- whichever source markup format?
--
-- This format should:
--     - format = read $ write format
--     - be able to encode metadata
--         - typeless data (strings)
--         - 1D lists of typeless data (strings)
--
-- 1. build metadata
-- 2. subsitute metadata
-- 3. convert tail (minus metadata block) to html

type Metadata = [Pair]

type Pair = ([Char], Value) -- string key, value

data Value = Value Char | ValueList [Char] -- string or 1D list of strings

-- with implicit grammar rules
parseMetadata :: Seq Block -> Maybe Metadata
parseMetadata blocks = case viewl blocks of
  Header 3 _ [Str "lumea-metadata"] :< blocks' -> parseBulletList blocks'
  _ -> Nothing

parseBulletList :: Seq Block -> Maybe Metadata
parseBulletList blocks = case viewl blocks of
  BulletList [items] :< _ -> parseListItems $ Data.Sequence.fromList items
  _ -> Nothing

-- pairs can be key: value or key: list of values
-- just worry about non-lists for now
parseListItems :: Seq Block -> Maybe Metadata
parseListItems items = case viewl items of
  Plain [key, space, value] :< items' ->
        -- combine this step's metadata construction
        -- continue parsing the item sequence
        -- Maybe Metadata -> Maybe Metadata -> Maybe Metadata
        -- where: l is Nothing -> nothing
        --        just l && just r -> Just l cons r
        --        _ -> Just l
        Nothing
  _ -> Nothing

-- grammar rules
isKey :: Inline -> Bool
isKey i = case i of
  Str text -> Data.Text.last text == ':'
  _ -> False

isValue :: Inline -> Bool
isValue i = case i of
  Str _ -> True
  _ -> False

isLumeaMetadataBlock :: [Block] -> Bool
isLumeaMetadataBlock bs = case bs of
  [Header 3 _ [Str "lumea-metadata"], BulletList _] -> True
  _ -> False

-- subject to change
-- will this support all the meta data I need?
-- this is clunky...
-- what if it wasn't...
exampleMetadataBlocks :: Blocks
exampleMetadataBlocks =
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
