{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.IRI where

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
parsePath = A.option "/" ((A.char '/') *> A.takeWhile1 isPath)
    where isPath c = (isIRI c) && (c /= '?') && (c /= '#')

parseQuery :: A.Parser (Maybe T.Text)
parseQuery = A.option Nothing (Just <$> ((A.char '?') *> A.takeWhile1 isQuery))
    where isQuery c = (isIRI c) && (c/= '#')

parseFragment :: A.Parser (Maybe T.Text)
parseFragment = A.option Nothing (Just <$> ((A.char '#') *> A.takeWhile1 isIRI))
