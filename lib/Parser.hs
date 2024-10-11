{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Monad (void)
import           Text.Parsec
import           Text.Parsec.String (Parser)
import           Data.Functor (($>))

-- util --
forceParse :: Parser a -> String -> a
forceParse parser string = case parse parser "" string of
  Left e       -> error $ show e
  Right parsed -> parsed

parse' :: Parser a -> String -> Either ParseError a
parse' parser = parse parser ""

many1Till :: Parser a -> Parser b -> Parser [a]
many1Till p end = (:) <$> p <*> manyTill p end

-- Org syntax reference: https://orgmode.org/worg/org-syntax.html
-- Parsec docs:
-- https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec.html#t:Parsec
data Object = Bold [Object]
            | Italic [Object]
            | Underline [Object]
            | StrikeThrough [Object]
            | Verbatim PlainText
            | Code PlainText
            | OfPlainText PlainText
            | LineBreak
            | Trivial -- testing
  deriving (Show, Eq)

newtype PlainText = PlainText String
  deriving (Show, Eq)

-- includes all objects excluding the default object: plainText
objects :: [Parser Object]
objects = [lineBreak]

-- for testing purposes
trivial :: Parser Object
trivial = return Trivial

lineBreak :: Parser Object
lineBreak = string "\\\\" *> space' *> (void newline <|> eof) $> LineBreak
  where
    space' = many (space <|> tab)

plainText :: Parser Object
plainText = OfPlainText . PlainText . unwords . words
  <$> many1Till anyChar (void (choice objects) <|> eof)
-- textMarkup :: ([Object] -> Object) -> Char -> Parser Object
-- textMarkup constructor marker = pre *> marker' *> contents
--   where
--     pre = void (many1 space)
--       <|> (void . choice $ char <$> ['-', '(', '{', '\'', '"'])
--       <|> void (lookAhead marker') <?> "prefix"
--     marker' = char marker <?> "marker"
--     contents
--       = notFollowedBy space
--       -- *> delay constructing objects may help?
--       *> (constructor <$> objectList) <?> "contents"
--     post = pure () <?> "postfix"
