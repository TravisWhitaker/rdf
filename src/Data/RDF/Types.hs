{-|
Module      : Data.RDF.Types
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides types for representing RDF data based on the abstract
syntax described in RDF 1.1 Concepts and Abstract Syntax.
-}

{-# LANGUAGE BangPatterns
           , DeriveGeneric
           , DeriveAnyClass
           #-}

module Data.RDF.Types (
    -- * Graphs
    RDFGraph(..)
  , Quad(..)
  , Triple(..)
    -- * Triple Components
  , Subject(..)
  , Predicate(..)
  , Object(..)
    -- * Terms
  , BlankNode(..)
  , Literal(..)
  , LiteralType(..)
    -- ** IRIs
  , IRI(..)
  , IRIAuth(..)
  ) where

import Control.DeepSeq

import GHC.Generics

import qualified Data.Text as T

-- | A contiguous RDF graph with optional label. Note that a contiguous graph
--   within an RDF data set will not appear as a single contiguous graph to this
--   library if the graph's constituent triples are not contiguous in the
--   original data set. This strategy allows for incremental processing of RDF
--   data in constant space.
data RDFGraph = RDFGraph {
    -- | A named RDF graph includes an 'IRI'.
    graphLabel :: !(Maybe IRI)
    -- | The constituent triples. A proper graph is a strict set of triples
    --   (i.e. no duplicate nodes or edges), but this guarantee cannot be made
    --   if the triples are to be processed incrementally in constant space.
    --   Programs using this type for interpreting RDF graphs should ignore any
    --   supernumerary triples in this list.
  , triples    :: [Triple]
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | An RDF quad, i.e. a triple belonging to a named graph.
data Quad = Quad {
    quadTriple :: !Triple
  , quadGraph  :: !(Maybe IRI)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | An RDF triple.
data Triple = Triple !Subject !Predicate !Object
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     , Generic
                     , NFData
                     )

-- | An RDF subject, i.e. either an 'IRI' or a 'BlankNode'.
data Subject = IRISubject   !IRI
             | BlankSubject !BlankNode
             deriving ( Eq
                      , Ord
                      , Read
                      , Show
                      , Generic
                      , NFData
                      )

-- | An RDF predicate.
newtype Predicate = Predicate { unPredicate :: IRI }
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           , Generic
                           , NFData
                           )

-- | An RDF object, i.e. either an 'IRI', a 'Literal', or a 'BlankNode'.
data Object = IRIObject     !IRI
            | LiteralObject !Literal
            | BlankObject   !BlankNode
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     , Generic
                     , NFData
                     )

-- | A blank node with its local label, without the preceeding "_:". Other
--   programs processing RDF are permitted to discard these node labels, i.e.
--   all blank node labels are local to a specific representation of an RDF data
--   set.
newtype BlankNode = BlankNode { unBlankNode :: T.Text }
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           , Generic
                           , NFData
                           )

-- | An RDF literal. As stipulated by the RDF standard, the 'litType' is merely
--   metadata; all RDF processing programs must try to handle literals that are
--   ill-typed.
data Literal = Literal {
    litString :: !T.Text
  , litType   :: !LiteralType
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | An RDF literal type. As stipulated by the RDF standard, this is merely
--   metadata; all RDF processing programs must try to handle literals that are
--   ill-typed.
data LiteralType = LiteralIRIType  !IRI
                 | LiteralLangType !T.Text
                 | LiteralUntyped
                 deriving ( Eq
                          , Ord
                          , Read
                          , Show
                          , Generic
                          , NFData
                          )

-- | An Internationalized Resource Identifier. This library preferentially
--   follows RFC 3987 over the RDF 1.1 specification, as the two standards
--   disagree about precisely what constitutes an IRI. A notable exception is
--   the handling of IRI fragments; this library follows the RDF 1.1
--   specification, allowing IRI fragments to occur in absolute IRIs, even
--   though this is expressly prohibited by RFC 3987.
--
--   Unlike the @network-uri@ package's behavior with URI fields, this library
--   does not include the sentinel tokens in the parsed fields. For example,
--   when parsing @http://example.com@, @network-uri@ will provide the string
--   @http:@ as the scheme, while this library will provide @http@ as the
--   scheme.
data IRI = IRI {
    -- | The IRI scheme, e.g. @http@
    iriScheme   :: !T.Text
    -- | The IRI authority, e.g. @example.com@
  , iriAuth     :: !(Maybe IRIAuth)
    -- | The IRI path, e.g. @/posts//index.html@
  , iriPath     :: !T.Text
    -- | The IRI path, i.e. the component after the @?@ if present.
  , iriQuery    :: !(Maybe T.Text)
    -- | The IRI fragment, i.e. the component after the @#@ if present.
  , iriFragment :: !(Maybe T.Text)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | An IRI Authority, as described by RFC 3987.
data IRIAuth = IRIAuth {
    -- | The IRI user, i.e. the component before the @\@@ if present.
    iriUser :: !(Maybe T.Text)
    -- | The IRI host, e.g. @example.com@.
  , iriHost :: T.Text
    -- | The IRI port, i.e. the numeral after the @:@ if present.
  , iriPort :: !(Maybe T.Text)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )
