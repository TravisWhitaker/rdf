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
import Data.RDF.Parser.Common

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
