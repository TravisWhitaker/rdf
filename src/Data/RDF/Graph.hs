{-|
Module      : Data.RDF.Graph
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides conversion between RDF triples and @fgl@ graphs. Naturally
these functions will force the entire graph into memory.
-}

{-# LANGUAGE DeriveGeneric
           , DeriveAnyClass
           #-}

module Data.RDF.Graph (
    -- FGL Supporting Types
    GNode(..)
  , GEdge
    -- * Conversion to FGL Graphs
  , rdfGraph
  , triplesGraph
    -- * Conversion from FGL Graphs
  , graphRDF
  , graphTriples
  ) where

import Control.DeepSeq

import qualified Data.Graph.Inductive.Graph   as G
import qualified Data.Graph.Inductive.NodeMap as G

import Data.Maybe

import Data.RDF.Types

import GHC.Generics

-- | An RDF 'Subject' or 'Object' as a 'G.Graph' node. This common
--   representation is necessary because the 'Object' of one 'Triple' might be
--   the 'Subject' of another.
data GNode = IRIGNode     !IRI
           | BlankGNode   !BlankNode
           | LiteralGNode !Literal
          deriving ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   , NFData
                   )

-- | A 'G.Graph' edge is an RDF 'Predicate'.
type GEdge = Predicate

-- | Convert a 'Subject' to a 'GNode'.
subjectNode :: Subject -> GNode
subjectNode (IRISubject i)   = IRIGNode i
subjectNode (BlankSubject b) = BlankGNode b

-- | Convert an 'Object' to a 'GNode'.
objectNode :: Object -> GNode
objectNode (IRIObject i)     = IRIGNode i
objectNode (BlankObject b)   = BlankGNode b
objectNode (LiteralObject l) = LiteralGNode l

-- | Convert a 'GNode' to a 'Subject'. This will fail if the 'GNode' contains a
--   'Literal'.
nodeSubject :: GNode -> Either String Subject
nodeSubject (IRIGNode i)   = Right (IRISubject i)
nodeSubject (BlankGNode b) = Right (BlankSubject b)
nodeSubject _              = Left "nodeSubject: subject must IRI or blank node."

-- | Convert a 'GNode' to an 'Object'.
nodeObject :: GNode -> Object
nodeObject (IRIGNode i)     = IRIObject i
nodeObject (BlankGNode b)   = BlankObject b
nodeObject (LiteralGNode l) = LiteralObject l

-- | Convert an 'RDFGraph' into a 'G.DynGraph' and 'G.NodeMap'. The 'graphLabel'
--   is discarded.
rdfGraph :: G.DynGraph g => RDFGraph -> (g GNode GEdge, G.NodeMap GNode)
rdfGraph (RDFGraph _ ts) = triplesGraph ts

-- | Convert a list of 'Triple's into a 'G.DynGraph' and a 'G.NodeMap'.
triplesGraph :: G.DynGraph g => [Triple] -> (g GNode GEdge, G.NodeMap GNode)
triplesGraph triples = G.mkMapGraph nodes edges
    where (nodes, edges)                = go ([],[]) triples
          go (ns, es) []                = (ns, es)
          go (ns, es) (Triple s p o:ts) = let s' = subjectNode s
                                              o' = objectNode o
                                          in go (s':o':ns, (s', o', p):es) ts

-- | Convert a 'G.Graph' into an 'RDFGraph'. This will fail if the graph
--   contains any 'LiteralGNode's with an outward degree greater than zero,
--   since such a graph is illegal in RDF.
graphRDF :: G.Graph g => (Maybe IRI) -> g GNode GEdge -> Either String RDFGraph
graphRDF l = (RDFGraph l <$>) .  graphTriples

-- | Convert a 'G.Graph' into a list of 'Triple's. This will fail if the graph
--   contains any 'LiteralGNode's with an outward degree greater than zero,
--   since such a graph is illegal in RDF.
graphTriples :: G.Graph g => g GNode GEdge -> Either String [Triple]
graphTriples g = go (G.labEdges g)
          -- The use of fromJust is safe here, since labEdges will never return
          -- an edge to a node not present in the graph.
    where go []               = Right []
          go ((si, oi, p):ts) = let s = nodeSubject (fromJust (G.lab g si))
                                    o = nodeObject (fromJust (G.lab g oi))
                                in ((\s' -> (Triple s' p o:)) <$> s) <*> go ts
