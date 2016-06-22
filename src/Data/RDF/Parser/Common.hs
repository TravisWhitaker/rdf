{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.Common where

import Control.Applicative

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text       as A

import Data.Char

import Data.RDF.Types

import qualified Data.Text as T

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

parseIRI :: A.Parser IRI
parseIRI = IRI <$> (parseScheme <* A.char ':')
               <*> parseAuth
               <*> parsePath
               <*> parseQuery
               <*> parseFragment

parseScheme :: A.Parser T.Text
parseScheme = T.toLower <$> (T.cons <$> A.letter <*> A.takeWhile1 isScheme)
    where isScheme c = (isAlphaNum c)
                    || (c == '+')
                    || (c == '-')
                    || (c == '.')

parseAuth :: A.Parser (Maybe IRIAuth)
parseAuth = A.option Nothing (A.string "//" *> (Just <$> parseIRIAuth))
    where parseIRIAuth = IRIAuth <$> parseUser
                                 <*> parseHost
                                 <*> parsePort

parseUser :: A.Parser (Maybe T.Text)
parseUser = A.option Nothing (Just <$> (A.takeWhile1 isUser <* A.char '@'))
    where isUser c = (isIRI c) && (c /= '@')

parseHost :: A.Parser T.Text
parseHost = A.takeWhile1 isHost
    where isHost c = (isIRI c) && (c /= '/') && (c /= ':')

parsePort :: A.Parser (Maybe T.Text)
parsePort = A.option Nothing (Just <$> (A.char ':' *> A.takeWhile1 isDigit))

parsePath :: A.Parser T.Text
parsePath = A.option "" ((A.char '/') *> A.takeWhile1 isPath)
    where isPath c = (isIRI c) && (c /= '?') && (c /= '#')

parseQuery :: A.Parser (Maybe T.Text)
parseQuery = A.option Nothing (Just <$> ((A.char '?') *> A.takeWhile1 isQuery))
    where isQuery c = (isIRI c) && (c/= '#')

parseFragment :: A.Parser (Maybe T.Text)
parseFragment = A.option Nothing (Just <$> ((A.char '#') *> A.takeWhile1 isIRI))

parseGraphLabel :: A.Parser (Maybe IRI)
parseGraphLabel = A.option Nothing (Just <$> parseEscapedIRI)

parseSubject :: A.Parser Subject
parseSubject = (IRISubject <$> parseEscapedIRI)
           <|> (BlankSubject <$> parseBlankNode)

parsePredicate :: A.Parser Predicate
parsePredicate = Predicate <$> parseEscapedIRI

parseObject :: A.Parser Object
parseObject = (IRIObject <$> parseEscapedIRI)
          <|> (BlankObject <$> parseBlankNode)
          <|> (LiteralObject <$> parseLiteral)

parseEscapedIRI :: A.Parser IRI
parseEscapedIRI = A.char '<' *> parseIRI <* A.char '>'

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
