{-# LANGUAGE OverloadedStrings #-}

module Meta where

import Data.Functor (($>))
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Yaml as Y
import qualified Text.Pandoc as P
import qualified Data.ByteString.Char8 as BS

test :: Parsec String () a -> String -> Either ParseError a
test parsec = parse parsec ""

dollarParser :: Parsec String () String
dollarParser = many $ noneOf ['$']

substituteParser :: Parsec String () String
substituteParser = char '$' *> many1 (noneOf ['$', ' ']) <* lookAhead (char '$')

-- need to wrap my head around this one :(
-- substitutesParser :: Parsec String () String
-- parse to dollar
--   success -> try substituteParser <> recurse
--   fail -> pure []

-- Format:
-- yaml code block containing metadata
-- rest of markdown format
-- read -> extract arbitrary metadata -> filter substitutions -> filter metadata block -> write

newtype Metadata = Metadata (M.Map String String) deriving (Show)

instance Y.FromJSON Metadata where
  parseJSON = Y.withObject "Metadata" (\o -> Metadata <$> o Y..: "lumea-metadata")

getMetaData :: String -> Metadata
getMetaData s = case Y.decodeEither' $ BS.pack s of
  Left e -> error $ Y.prettyPrintParseException e
  Right metadata -> metadata

-- substitute $key$ from metadata
-- filterSubstitute :: P.Inline -> P.Inline
