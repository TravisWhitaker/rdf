{-|
Module      : Data.RDF.ToRDF
Description : DSL for Mapping Haskell Data to RDF Graphs
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

This module provides a simple DSL for mapping Haskell data to RDF graphs.
-}

{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TupleSections
           #-}

module Data.RDF.ToRDF (
    ToRDF(..)
  , ToObject(..)
  , toTriples
  , Triples
  , RDFGen
  , runRDFGen
  , appBaseIRI
  , newBlankNode
  ) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import qualified Data.DList as DL

import Data.Int

import Data.Monoid

import Data.RDF.Types

import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TL
import qualified Data.Text.Lazy.Builder.Int       as TL
import qualified Data.Text.Lazy.Builder.RealFloat as TL

import Data.Word

type Triples = DL.DList Triple

-- | RDF generator monad. Provides 'ReaderT' for the base 'IRI', and 'StateT'
--   for a monotonically increasing blank node identifier.
type RDFGen a = ReaderT IRI (State Word64) a

runRDFGen :: RDFGen a -> IRI -> a
runRDFGen m i = evalState (runReaderT m i) 0

class ToRDF a where
    triples :: a -> RDFGen Triples

class ToObject a where
    object :: a -> RDFGen Object

instance ToObject Int where
    object = pure . toLObject . TL.decimal

instance ToObject Integer where
    object = pure . toLObject . TL.decimal

instance ToObject Int8 where
    object = pure . toLObject . TL.decimal

instance ToObject Int16 where
    object = pure . toLObject . TL.decimal

instance ToObject Int32 where
    object = pure . toLObject . TL.decimal

instance ToObject Int64 where
    object = pure . toLObject . TL.decimal

instance ToObject Word where
    object = pure . toLObject . TL.decimal

instance ToObject Word8 where
    object = pure . toLObject . TL.decimal

instance ToObject Word16 where
    object = pure . toLObject . TL.decimal

instance ToObject Word32 where
    object = pure . toLObject . TL.decimal

instance ToObject Word64 where
    object = pure . toLObject . TL.decimal

instance ToObject String where
    object s = pure $ LiteralObject (Literal (T.pack s) LiteralUntyped)

instance ToObject T.Text where
    object t = pure $ LiteralObject (Literal t LiteralUntyped)

-- | Forces the lazy 'TL.Text'.
instance ToObject TL.Text where
    object t = pure $ LiteralObject (Literal (TL.toStrict t) LiteralUntyped)

instance ToObject Float where
    object = pure . toLObject . TL.realFloat

instance ToObject Double where
    object = pure . toLObject . TL.realFloat

toTriples :: ToRDF a => IRI -> a -> [Triple]
toTriples i x = DL.toList (runRDFGen (triples x) i)

toText :: TL.Builder -> T.Text
toText = TL.toStrict . TL.toLazyText

toLObject :: TL.Builder -> Object
toLObject b = LiteralObject (Literal (toText b) LiteralUntyped)

appBaseIRI :: Endo IRI -> RDFGen IRI
appBaseIRI = asks . appEndo

newBlankNode :: RDFGen BlankNode
newBlankNode = ReaderT (const ((BlankNode . toText . TL.decimal)
                           <$> get <* modify' (+1)))
