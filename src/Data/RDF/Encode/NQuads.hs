{-|
Module      : Data.RDF.Encode.NQuads
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

An encoder for
<https://www.w3.org/TR/2014/REC-n-quads-20140225/ RDF 1.1 N-Quads>.
'B.Builder's are used to support efficient incremental output.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Encode.NQuads (
    -- * Graph Encoding
    encodeRDFGraph
  , encodeRDFGraphs
  ) where

import qualified Data.ByteString.Builder as B

import Data.Monoid

import Data.RDF.Types
import Data.RDF.Encode.Common

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

-- | Encodes a 'Triple' as a single line, i.e. with no graph label. Includes the
--   terminating period and newline.
encodeTriple :: Triple -> B.Builder
encodeTriple (Triple s p o) = encodeSubject s
                           <> B.byteString " "
                           <> encodePredicate p
                           <> B.byteString " "
                           <> encodeObject o
                           <> B.byteString " .\n"

-- | Encodes a 'Quad' as a single line. Includes the terminating period and
--   newline.
encodeQuad :: Quad -> B.Builder
encodeQuad (Quad t Nothing)               = encodeTriple t
encodeQuad (Quad (Triple s p o) (Just g)) = encodeSubject s
                                         <> B.byteString " "
                                         <> encodePredicate p
                                         <> B.byteString " "
                                         <> encodeObject o
                                         <> B.byteString " "
                                         <> encodeEscapedIRI g
                                         <> B.byteString " .\n"

-- | Encode a single 'RDFGraph' as a 'B.Builder'.
encodeRDFGraph :: RDFGraph -> B.Builder
encodeRDFGraph (RDFGraph Nothing ts)  = mconcat $ map encodeTriple ts
encodeRDFGraph (RDFGraph (Just g) ts) = let qs = map (\t -> Quad t (Just g)) ts
                                        in mconcat $ map encodeQuad qs

-- | Encode multiple 'RDFGraph's as a 'B.Builder'.
encodeRDFGraphs :: Foldable f => f RDFGraph -> B.Builder
encodeRDFGraphs = foldMap encodeRDFGraph
