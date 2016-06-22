{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Encode.NQuads where

import qualified Data.ByteString.Builder as B

import Data.Monoid

import Data.RDF.Types
import Data.RDF.Encode.Common

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

-- | Encodes a 'Triple' as a single line, i.e. with no graph label.
encodeTriple :: Triple -> B.Builder
encodeTriple (Triple s p o) = mconcat [ encodeSubject s
                                      , B.byteString " "
                                      , encodePredicate p
                                      , B.byteString " "
                                      , encodeObject o
                                      , B.byteString " .\n"
                                      ]

encodeQuad :: Quad -> B.Builder
encodeQuad (Quad t Nothing)               = encodeTriple t
encodeQuad (Quad (Triple s p o) (Just g)) = mconcat
    [ encodeSubject s
    , B.byteString " "
    , encodePredicate p
    , B.byteString " "
    , encodeObject o
    , B.byteString " "
    , encodeEscapedIRI g
    , B.byteString " .\n"
    ]

encodeRDFGraph :: RDFGraph -> B.Builder
encodeRDFGraph (RDFGraph Nothing ts)  = mconcat $ map encodeTriple ts
encodeRDFGraph (RDFGraph (Just g) ts) = let qs = map (\t -> Quad t (Just g)) ts
                                        in mconcat $ map encodeQuad qs

encodeRDFGraphs :: Foldable f => f RDFGraph -> B.Builder
encodeRDFGraphs = foldMap encodeRDFGraph
