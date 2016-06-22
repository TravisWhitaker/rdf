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

module Data.RDF.Parser.NQuads (
    Result(..)
  , parseNQuads
  ) where

import Control.Applicative

import qualified Data.Attoparsec.Text      as A
import qualified Data.Attoparsec.Text.Lazy as AL

import Data.Char

import Data.List

import Data.RDF.Types
import Data.RDF.Parser.Common

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

-- | Either an 'RDFGraph' or a parse error.
type Result = Either String RDFGraph

-- | A parser for
--   <https://www.w3.org/TR/2014/REC-n-quads-20140225/ RDF 1.1 N-Quads>. This
--   parser works incrementally by first lazily splitting the input into lines,
--   then parsing each line of the N-Quads document individually. This allows
--   for incremental processing in constant space, as well as extracting any
--   valid data from an N-Quads document that contains some invalid quads.
--   'LT.Text' is used because the RDF 1.1 specification stipulates that RDF
--   should always be encoded with Unicode.
--
--   Due to its incremental nature, this parser will accept some N-Quads
--   documents that are not legal according to the RDF 1.1 specification.
--   Specifically, this parser will provide duplicate 'Triple's if they exist in
--   the input N-Quads document; a proper graph consists of true sets of nodes
--   and edges, i.e. no duplicate nodes or edges. Any downstream program
--   incrementally consuming this parser's output should take care to ignore any
--   supernumary triples.
--
--   Likewise, if a graph's constituent triples are not contiguous in the input
--   N-Quads document, then they will not be folded into contiguous 'RDFGraph's
--   in this parser's output. Any downstream program incrementally consuming
--   this parser's output and performing graph processing that discriminates
--   based on graph labels will not necessarily be presented each contiguous
--   labeled graph as a single 'RDFGraph' record. For example, something like
--   this could be used to lazily find all 'RDFGraph' records containing a named
--   graph's 'Triple's. Downstream processing must then be able to handle a
--   single named graph spanning multiple 'RDFGraph' records.
--
-- > filterGraph :: (Maybe IRI) -> [RDFGraph] -> [RDFGraph]
-- > filterGraph gl = filter (\g -> (graphLabel g) == gl)
parseNQuads :: LT.Text -> [Result]
parseNQuads = foldGraphs
            . map (AL.eitherResult . (AL.parse parseQuad))
            . LT.lines

-- | Fold a list of parsed 'Quad's into a list of parsed 'RDFGraphs', where
--   adjacent 'Quad's in the input are included in the same 'RDFGraph'.
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

-- | Parse a single N-Quads 'Triple'.
parseTriple :: A.Parser Triple
parseTriple = Triple <$> (parseSubject <* A.skipSpace)
                     <*> (parsePredicate <* A.skipSpace)
                     <*> parseObject

-- | Parse a single N-Quads 'Quad'.
parseQuad :: A.Parser Quad
parseQuad = Quad <$> parseTriple
                 <*> ((A.skipSpace *> parseGraphLabel) <*
                     (A.skipSpace *> A.char '.'))
