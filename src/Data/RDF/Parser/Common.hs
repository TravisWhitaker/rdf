{-|
Module      : Data.RDF.Parser.Common
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides parsers for the primitive terms in the RDF abstract syntax
as described in RDF 1.1 Concepts and Abstract Syntax. These should be useful for
all RDF host languages.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.Common (
    -- * Triple Components
    parseSubject
  , parsePredicate
  , parseObject
  , parseGraphLabel
    -- * Terms
  , parseBlankNode
  , parseLiteral
    -- ** IRIs
  , parseIRI
  , parseEscapedIRI
  ) where

import Control.Applicative

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text       as A

import Data.Char

import Data.RDF.Types

import qualified Data.Text as T

-- | Predicate on 'Char's for acceptability for inclusion in an 'IRI'.
isIRI :: Char -> Bool
isIRI c = (c /= '<')
       && (c /= '>')
       && (c /= '"')
       && (c /= '{')
       && (c /= '}')
       && (c /= '|')
       && (c /= '^')
       && (c /= '`')
       && (c /= '\\')

-- | 'IRI' parser.
parseIRI :: A.Parser IRI
parseIRI = IRI <$> (parseScheme <* A.char ':')
               <*> parseAuth
               <*> parsePath
               <*> parseQuery
               <*> parseFragment

-- | 'IRI' scheme parser.
parseScheme :: A.Parser T.Text
parseScheme = T.toLower <$> (T.cons <$> A.letter <*> A.takeWhile1 isScheme)
    where isScheme c = (isAlphaNum c)
                    || (c == '+')
                    || (c == '-')
                    || (c == '.')

-- | 'IRIAuth' parser.
parseAuth :: A.Parser (Maybe IRIAuth)
parseAuth = A.option Nothing (A.string "//" *> (Just <$> parseIRIAuth))
    where parseIRIAuth = IRIAuth <$> parseUser
                                 <*> parseHost
                                 <*> parsePort

-- | 'IRIAuth' user parser.
parseUser :: A.Parser (Maybe T.Text)
parseUser = A.option Nothing (Just <$> (A.takeWhile1 isUser <* A.char '@'))
    where isUser c = (isIRI c) && (c /= '@')

-- | 'IRIAuth' host parser.
parseHost :: A.Parser T.Text
parseHost = A.takeWhile1 isHost
    where isHost c = (isIRI c) && (c /= '/') && (c /= ':')

-- | 'IRIAuth' port parser.
parsePort :: A.Parser (Maybe T.Text)
parsePort = A.option Nothing (Just <$> (A.char ':' *> A.takeWhile1 isDigit))

-- | 'IRI' path parser.
parsePath :: A.Parser T.Text
parsePath = A.option "" ((A.char '/') *> A.takeWhile1 isPath)
    where isPath c = (isIRI c) && (c /= '?') && (c /= '#')

-- | 'IRI' query parser.
parseQuery :: A.Parser (Maybe T.Text)
parseQuery = A.option Nothing (Just <$> ((A.char '?') *> A.takeWhile1 isQuery))
    where isQuery c = (isIRI c) && (c/= '#')

-- | 'IRI' fragment parser.
parseFragment :: A.Parser (Maybe T.Text)
parseFragment = A.option Nothing (Just <$> ((A.char '#') *> A.takeWhile1 isIRI))

-- | Parser for graph labels, i.e. either an escaped 'IRI' or the empty string.
parseGraphLabel :: A.Parser (Maybe IRI)
parseGraphLabel = A.option Nothing (Just <$> parseEscapedIRI)

-- | 'Subject' parser.
parseSubject :: A.Parser Subject
parseSubject = (IRISubject <$> parseEscapedIRI)
           <|> (BlankSubject <$> parseBlankNode)

-- | 'Predicate' parser.
parsePredicate :: A.Parser Predicate
parsePredicate = Predicate <$> parseEscapedIRI

-- | 'Object' parser.
parseObject :: A.Parser Object
parseObject = (IRIObject <$> parseEscapedIRI)
          <|> (BlankObject <$> parseBlankNode)
          <|> (LiteralObject <$> parseLiteral)

-- | Parse an escaped 'IRI', i.e. an IRI enclosed in angle brackets.
parseEscapedIRI :: A.Parser IRI
parseEscapedIRI = A.char '<' *> parseIRI <* A.char '>'

-- | Parse a blank node label.
parseBlankNode :: A.Parser BlankNode
parseBlankNode = BlankNode <$> (A.string "_:" *> label)
    where label       = T.cons <$> labelHead <*> (A.option T.empty labelBody)
          labelHead   = A.satisfy isHead
          labelBody   = (A.takeWhile isLabel) >>= checkEnd
          checkEnd t
                | T.null t        = pure t
                | T.last t /= '.' = pure t
                | otherwise       = fail "label must not end with '.'"
          isLabel     = not . isSpace
          isHead c    = (isLabel c)
                     && (c /= '-')
                     && (c /= '.')
          isTail c    = (isLabel c)
                     && (c /= '.')

-- | Parse an RDF 'Literal', including the 'LiteralType' if present.
parseLiteral :: A.Parser Literal
parseLiteral = Literal <$> litVal <*> valType
    where litVal      = A.char '"' *> escString
          valType     = valIRIType <|> valLangType <|> pure LiteralUntyped
          valIRIType  = LiteralIRIType <$> (A.string "^^" *> parseEscapedIRI)
          valLangType = LiteralLangType <$> (A.char '@' *> A.takeWhile1 isLang)
          isLang c    = (isAlphaNum c) || (c == '-')
          escString   = do
                c <- A.anyChar
                case c of '"'  -> pure T.empty
                          '\\' -> T.cons <$> (res <$> A.anyChar) <*> escString
                          _    -> T.cons c <$> escString
          res 't' = '\t'
          res 'b' = '\b'
          res 'n' = '\n'
          res 'r' = '\r'
          res 'f' = '\f'
          res c   = c
