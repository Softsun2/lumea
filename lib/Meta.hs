{-# LANGUAGE OverloadedStrings #-}

module Meta where

import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Data.Yaml as Y
import qualified Text.Pandoc as P
import Text.Pandoc.Parsing (many1Till)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token

seekDollar :: Parser String
seekDollar = manyTill anyChar $ lookAhead $ char '$'

substitute :: Parser String
substitute =
  char '$'
    *> many1Till anyChar (try $ choice (fmap lookAhead [char '$', space]))
    <* lookAhead (char '$')

-- todo: simplify me
substitutes :: Parser [String]
substitutes =
  try seekDollar
    *> ( ((:) <$> try substitute <*> substitutes) -- try to capture substitute
           <|> (char '$' *> substitutes) -- otherwise continue
       )
    <|> return []

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