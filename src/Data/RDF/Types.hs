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

import Data.RDF.Internal
