{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad (void)
import           Data.Functor (($>))
import           Text.Parsec
import           Text.Parsec.String (Parser)

parse' :: Parser a -> String -> a
parse' parser string = case parse parser "" string of
  Left e       -> error $ show e
  Right parsed -> parsed

-- Org syntax reference: https://orgmode.org/worg/org-syntax.html
-- Parsec docs:
-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#t:Parsec
-- Note: Org parse data types are potential candidates for data
-- herding: https://youtu.be/YR5WdGrpoug?si=vTti5RTBWGZwaUX8
-- objects:
-- - plain text
-- - text markup
-- - entities
-- - LaTeX fragments
-- - super and subscripts
data Object = TextMarkup [Object]
            | PlainText String
  deriving (Show)

textMarkup :: Char -> Parser Object
textMarkup marker = pre *> marker' *> contents <* marker' <* post
  where
    marker' = char marker

    pre = choice
      [ void $ many1 space
      , void . choice $ char <$> ['-', '(', '{', '\'', '"']
      , void $ lookAhead marker']

    -- contents may not begin or end with whitespace
    contents = manyTill anyChar (lookAhead marker')
      *> if marker == '=' || marker == '~'
         then return $ PlainText "verbatim/code"
         else return $ PlainText "sequence of objects"

    post = eof
      <|> (choice
             [ void space
             , void . choice
               $ char
               <$> [ '-'
                   , '.'
                   , ','
                   , ';'
                   , ':'
                   , '!'
                   , '?'
                   , '\''
                   , ')'
                   , '}'
                   , '['
                   , '"'
                   , '\\']]
           *> eof)

bold = textMarkup '*'

italic = textMarkup '/'

underline = textMarkup '_'

verbatim = textMarkup '='

code = textMarkup '~'

strikethrough = textMarkup '+'
-- elements
