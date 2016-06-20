{-|
Module      : Data.RDF.Types
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

RDF types.
-}

{-# LANGUAGE BangPatterns
           , DeriveAnyClass
           #-}

module Data.RDF.Types where

import qualified Data.Text as T

data RDFGraph = RDFGraph {
    graphLabel :: !(Maybe IRI)
  , triples    :: [Triple]
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )

data Quad = Quad {
    quadTriple :: !Triple
  , quadGraph  :: !(Maybe IRI)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )

data Triple = Triple !Subject !Predicate !Object
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     )

data Subject = IRISubject   !IRI
             | BlankSubject !BlankNode
             deriving ( Eq
                      , Ord
                      , Read
                      , Show
                      )

newtype Predicate = Predicate { unPredicate :: IRI }
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           )

data Object = IRIObject     !IRI
            | LiteralObject !Literal
            | BlankObject   !BlankNode
            deriving ( Eq
                     , Ord
                     , Read
                     , Show
                     )

-- | A blank node with it's local label, without the preceeding "_:".
newtype BlankNode = BlankNode { unBlankNode :: T.Text }
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           )

data Literal = Literal {
    litString :: !T.Text
  , litType   :: !(Maybe IRI)
  , litLang   :: !(Maybe T.Text)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )

data IRI = IRI {
    iriScheme   :: !T.Text
  , iriAuth     :: !(Maybe IRIAuth)
  , iriPath     :: !T.Text
  , iriQuery    :: !(Maybe T.Text)
  , iriFragment :: !(Maybe T.Text)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )

data IRIAuth = IRIAuth {
    iriUser :: !(Maybe T.Text)
  , iriHost :: T.Text
  , iriPort :: !(Maybe T.Text)
  } deriving ( Eq
             , Ord
             , Read
             , Show
             )
