{-# LANGUAGE OverloadedStrings #-}

module Meta
    ( insertMetadata
      -- , isDirty
      -- , touch
    , parseKeys -- public for testing
    ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List (nub)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Text.Pandoc as P
import           Text.Pandoc.Parsing (many1Till)
import           Text.Pandoc.Walk (Walkable(walk))
import           Text.Parsec
import           Text.Parsec.String (Parser)

newtype Metadata = Metadata (M.Map String String)
  deriving (Show)

instance Y.FromJSON Metadata where
  parseJSON =
    Y.withObject "Metadata" (\o -> Metadata <$> o Y..: "lumea-metadata")

insertMetadata :: P.Pandoc -> P.Pandoc
insertMetadata pandoc = case getMetadata pandoc of
  Nothing       -> pandoc
  Just metadata
    -> let P.Pandoc _ (_:blocks) = walk (filterInsertMetadata metadata) pandoc
       in P.Pandoc P.nullMeta blocks

parseKeys :: Parser [String]
parseKeys = try seekDollar
  *> (((:) <$> try parseKey <*> parseKeys) -- try to capture substitute
      <|> (char '$' *> parseKeys) -- otherwise continue
      )
  <|> return []

seekDollar :: Parser String
seekDollar = manyTill anyChar $ lookAhead $ char '$'

parseKey :: Parser String
parseKey = char '$'
  *> many1Till anyChar (try $ choice (fmap lookAhead [char '$', space]))
  <* lookAhead (char '$')

parseMetadata :: String -> Maybe Metadata
parseMetadata s = case Y.decodeEither' $ BS.pack s of
  Left _         -> Nothing
  Right metadata -> Just metadata

parse' :: Parser a -> String -> a
parse' parser string = case parse parser "" string of
  Left e       -> error $ show e
  Right parsed -> parsed

insertMetadataText :: Metadata -> T.Text -> T.Text
insertMetadataText (Metadata metadata) text =
  let keys :: [String]
      keys = nub $ parse' parseKeys $ T.unpack text
      f :: T.Text -> String -> T.Text
      f text' key = case M.lookup key metadata of
        Nothing    -> text'
        Just value
          -> T.replace (T.pack $ "$" <> key <> "$") (T.pack value) text'
  in foldl f text keys

filterInsertMetadata :: Metadata -> P.Inline -> P.Inline
filterInsertMetadata metadata (P.Str text) =
  P.Str $ insertMetadataText metadata text
filterInsertMetadata _ i = i

getMetadata :: P.Pandoc -> Maybe Metadata
getMetadata pandoc = case pandoc of
  P.Pandoc _ ((P.CodeBlock _ code):_) -> parseMetadata $ T.unpack code
  _ -> Nothing
