{-|
Module      : Data.RDF.Parser.Common
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides parsers for the primitive terms in the RDF abstract syntax
as described in RDF 1.1 Concepts and Abstract Syntax. These should be useful for
all RDF host languages.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Parser.Common (
    -- * Triple Components
    parseSubject
  , parsePredicate
  , parseObject
  , parseGraphLabel
    -- * Terms
  , parseBlankNode
  , parseLiteral
    -- ** IRIs
  , parseIRI
  , parseEscapedIRI
  ) where

import Data.RDF.Internal
