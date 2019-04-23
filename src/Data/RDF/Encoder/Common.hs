{-|
Module      : Data.RDF.Encode.Common
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides encoders for the primitive terms in the RDF abstract syntax
as described in RDF 1.1 Concepts and Abstract Syntax. These should be useful for
all RDF host languages.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.RDF.Encoder.Common (
    -- * Triple Components
    encodeSubject
  , encodePredicate
  , encodeObject
    -- * Terms
  , encodeBlankNode
  , encodeLiteral
    -- ** IRIs
  , encodeIRI
  , encodeEscapedIRI
    -- * Utilities
  , quoteString
  , maybeBuilder
  ) where

import qualified Data.ByteString.Builder as B

import Data.RDF.Types

import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

-- | Escape the double quotes in a quoted string literal.
quoteString :: T.Text -> T.Text
quoteString = T.replace "\"" "\\\""

-- | Maps 'Nothing' to 'mempty'.
maybeBuilder :: Maybe B.Builder -> B.Builder
maybeBuilder Nothing  = mempty
maybeBuilder (Just b) = b

-- | Encode an escaped 'IRI', i.e. between angle brackets.
encodeEscapedIRI :: IRI -> B.Builder
encodeEscapedIRI i = B.byteString "<" <> encodeIRI i <> B.byteString ">"

-- | Encode an 'IRI'.
encodeIRI :: IRI -> B.Builder
encodeIRI (IRI s a p q f) = T.encodeUtf8Builder s
                         <> B.byteString ":"
                         <> maybeBuilder (encodeIRIAuth <$> a)
                         <> B.byteString "/"
                         <> T.encodeUtf8Builder p
                         <> maybeBuilder (((B.byteString "?" <>) . T.encodeUtf8Builder) <$> q)
                         <> maybeBuilder (((B.byteString "#" <>) . T.encodeUtf8Builder) <$> f)

-- | Encode an 'IRIAuth'.
encodeIRIAuth :: IRIAuth -> B.Builder
encodeIRIAuth (IRIAuth u h p) = B.byteString "//"
                             <> maybeBuilder (((<> B.byteString "@") . T.encodeUtf8Builder) <$> u)
                             <> T.encodeUtf8Builder h
                             <> maybeBuilder (((B.byteString ":" <>) . T.encodeUtf8Builder) <$> p)

-- | Encode a 'Literal', including the 'LiteralType'.
encodeLiteral :: Literal -> B.Builder
encodeLiteral (Literal v t) = B.byteString "\""
                           <> T.encodeUtf8Builder (quoteString v)
                           <> B.byteString "\""
                           <> encodeLiteralType t

-- | Encode a 'LiteralType'.
encodeLiteralType :: LiteralType -> B.Builder
encodeLiteralType (LiteralIRIType i)  = B.byteString "^^"
                                     <> encodeEscapedIRI i
encodeLiteralType (LiteralLangType l) = B.byteString "@"
                                     <> T.encodeUtf8Builder l
encodeLiteralType LiteralUntyped      = mempty

-- | Encode a 'BlankNode'.
encodeBlankNode :: BlankNode -> B.Builder
encodeBlankNode (BlankNode l) = B.byteString "_:" <> T.encodeUtf8Builder l

-- | Encode a 'Subject'.
encodeSubject :: Subject -> B.Builder
encodeSubject (IRISubject i)   = encodeEscapedIRI i
encodeSubject (BlankSubject b) = encodeBlankNode b

-- | Encode a 'Predicate'.
encodePredicate :: Predicate -> B.Builder
encodePredicate (Predicate i) = encodeEscapedIRI i

-- | Encode a 'Object'.
encodeObject :: Object -> B.Builder
encodeObject (IRIObject i)     = encodeEscapedIRI i
encodeObject (LiteralObject l) = encodeLiteral l
encodeObject (BlankObject b)   = encodeBlankNode b
