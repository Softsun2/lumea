{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

parse' :: Parser a -> String -> a
parse' parser string = case parse parser "" string of
  Left e -> error $ show e
  Right parsed -> parsed

-- Org syntax reference: https://orgmode.org/worg/org-syntax.html

-- Parsec docs:
-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#t:Parsec

-- Note: Org parse data types are potential candidates for data
-- herding: https://youtu.be/YR5WdGrpoug?si=vTti5RTBWGZwaUX8

data Heading = Heading
  { stars :: Int,
    -- keyword :: Maybe String,
    -- priority :: Maybe Int,
    -- comment :: Maybe String,
    title :: Maybe String,
    tags :: Maybe [String]
    -- also beleive it has a list of sections
  }
  deriving (Show, Eq)

-- todo: confirm Text.Parsec.spaces captures org space characters:
-- spaces, tabs, newlines, and line feeds
heading :: Parser Heading
heading = do
  -- consume stars followed by spaces
  stars <- (length <$> many1 (char '*')) <* many1 space
  return $ Heading stars (Just "title") (Just ["tag1", "tag2"])
