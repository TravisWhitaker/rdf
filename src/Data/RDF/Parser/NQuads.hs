{-|
Module      : Data.RDF.Parser.NQuads
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

A parser for <https://www.w3.org/TR/2014/REC-n-quads-20140225/ RDF 1.1 N-Quads>.

-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.NQuads where

import Control.Applicative

import qualified Data.Attoparsec.Text      as A
import qualified Data.Attoparsec.Text.Lazy as AL

import Data.Char

import Data.List

import Data.RDF.Types
import Data.RDF.Parser.IRI

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

type Result = Either String RDFGraph

parseNQuads :: LT.Text -> [Result]
parseNQuads = foldGraphs . map (AL.eitherResult . parseQuad') . LT.lines

foldGraphs :: [Either String Quad] -> [Either String RDFGraph]
foldGraphs [] = []
foldGraphs ((Left e):qs)  = (Left e) : foldGraphs qs
foldGraphs ((Right q):qs) = go (RDFGraph (quadGraph q) [(quadTriple q)]) qs
    where go g []            = [Right g]
          go g ((Left e):qs) = (Right g) : (Left e) : (foldGraphs qs)
          go g@(RDFGraph gl ts) ((Right q):qs)
                | gl == (quadGraph q) = go (RDFGraph gl ((quadTriple q):ts)) qs
                | otherwise           = (Right g) : go (RDFGraph (quadGraph q)
                                                       [(quadTriple q)]) qs

parseQuad' :: LT.Text -> AL.Result Quad
parseQuad' = AL.parse parseQuad

parseTriple :: A.Parser Triple
parseTriple = Triple <$> (parseSubject <* A.skipSpace)
                     <*> (parsePredicate <* A.skipSpace)
                     <*> parseObject

parseQuad :: A.Parser Quad
parseQuad = Quad <$> parseTriple
                 <*> ((A.skipSpace *> parseGraphLabel) <*
                     (A.skipSpace *> A.char '.'))

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
