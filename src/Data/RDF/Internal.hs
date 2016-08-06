{-|
Module      : Data.RDF.Internal
Description : Representation and Incremental Processing of RDF Data
Copyright   : Travis Whitaker 2016
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable

Internal module.
-}

{-# LANGUAGE DeriveGeneric
           , DeriveAnyClass
           , OverloadedStrings
           #-}

module Data.RDF.Internal where

import Control.Applicative

import Control.DeepSeq

import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Text       as A

import Data.Char

import Data.String

import GHC.Generics

import qualified Data.Text as T

-- | A contiguous RDF graph with optional label. Note that a contiguous graph
--   within an RDF data set will not appear as a single contiguous graph to this
--   library if the graph's constituent triples are not contiguous in the
--   original data set. This strategy allows for incremental processing of RDF
--   data in constant space.
data RDFGraph = RDFGraph {
    -- | A named RDF graph includes an 'IRI'.
    rdfLabel :: !(Maybe IRI)
    -- | The constituent triples. A proper graph is a strict set of triples
    --   (i.e. no duplicate nodes or edges), but this guarantee cannot be made
    --   if the triples are to be processed incrementally in constant space.
    --   Programs using this type for interpreting RDF graphs should ignore any
    --   supernumerary triples in this list.
  , rdfTriples    :: [Triple]
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
--
--   This type has an 'IsString' instance, allowing string literals to be
--   interpreted as 'Subject's with @-XOverloadedStrings@, like so:
--
--   >>> "<http://example.com> :: Subject
--   IRISubject (IRI (...))
--   >>> "_:some-node" :: Subject
--   BlankSubject (BlankNode {unBlankNode = "some-node"})
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
--
--   This type has an 'IsString' instance, allowing string literals to be
--   interpreted as 'Predicate's with @-XOverloadedStrings@, like so:
--
--   >>> "<http://example.com>" :: Predicate
--   Predicate {unPredicate = IRI (...)}
newtype Predicate = Predicate { unPredicate :: IRI }
                  deriving ( Eq
                           , Ord
                           , Read
                           , Show
                           , Generic
                           , NFData
                           )

-- | An RDF object, i.e. either an 'IRI', a 'Literal', or a 'BlankNode'.
--
--   This type has an 'IsString' instance, allowing string literals to be
--   interpreted as 'Object's with @-XOverloadedStrings@, like so:
--
--   >>> "<http://example.com>" :: Object
--   IRIObject (IRI (...))
--   >>> "_:some-node" :: Object
--   BlankObject (BlankNode {unBlankNode = "some-node"})
--   >>> "computer" :: Object
--   LiteralObject (Literal {litString = "computer", litType = LiteralUntyped})
--
--   The precedence for literal interpretation is IRI > BlankNode > Literal. To
--   force a literal that is also a valid blank node label or IRI to be
--   interpreted as a 'LiteralObject', wrap it in an extra set of double quotes:
--
--   >>> "\"_:some-node\"" :: Object
--   LiteralObject (Literal {litString = "_:some-node", litType = LiteralUntyped})
data Object = IRIObject     !IRI
            | BlankObject   !BlankNode
            | LiteralObject !Literal
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
--
--   This type has an 'IsString' instance, allowing string literals to be
--   interpreted as 'BlankNode's with @-XOverloadedStrings@, like so:
--
--   >>> "_:some-node" :: BlankNode
--   BlankNode {unBlankNode = "some-node"}
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
--
--   This type has an 'IsString' instance, allowing string literals to be
--   interpreted as 'Literal's with @-XOverloadedStrings@, like so:
--
--   >>> "computer" :: Literal
--   Literal {litString = "computer", litType = LiteralUntyped}
--
--   For untyped literals the extra double quotes are not required. They are
--   required for typed literals:
--
--   >>> "\"computer\"@en" :: Literal
--   Literal {litString = "computer", litType = LiteralLangType "en"}
--
--   >>> "\"computer\"^^<http://computer.machine/machine>" :: Literal
--   Literal { litString = "computer", litType = LiteralIRIType (...)}
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
--
--   This type has an 'IsString' instnace, allowing string literals to be
--   interpreted as 'IRI's with @-XOverloadedStrings@, like so:
--
--   >>> "http://example.com" :: IRI
--   IRI { iriScheme = "http"
--       , iriAuth = Just (IRIAuth { iriUser = Nothing
--                                 , iriHost = "example.com"
--                                 , iriPort = Nothing
--                                 })
--       , iriPath = ""
--       , iriQuery = Nothing
--       , iriFragment = Nothing
--       }
data IRI = IRI {
    -- | The IRI scheme, e.g. @http@
    iriScheme   :: !T.Text
    -- | The IRI authority, e.g. @example.com@
  , iriAuth     :: !(Maybe IRIAuth)
    -- | The IRI path, e.g. @/posts//index.html@
  , iriPath     :: !T.Text
    -- | The IRI query, i.e. the component after the @?@ if present.
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

-- | Predicate on 'Char's for acceptability for inclusion in an 'IRI'.
isIRI :: Char -> Bool
isIRI c = (c /= '<')
       && (c /= '>')
       && (c /= '"')
       && (c /= '{')
       && (c /= '}')
       && (c /= '|')
       && (c /= '^')
       && (c /= '`')
       && (c /= '\\')

-- | 'IRI' parser.
parseIRI :: A.Parser IRI
parseIRI = IRI <$> (parseScheme <* A.char ':')
               <*> parseAuth
               <*> parsePath
               <*> parseQuery
               <*> parseFragment

-- | 'IRI' scheme parser.
parseScheme :: A.Parser T.Text
parseScheme = A.takeWhile1 isScheme >>= check
    where check t
            | isAlpha (T.head t) = pure t
            | otherwise          = fail "parseScheme: must start with letter."
          isScheme c = isAlphaNum c
                    || (c == '+')
                    || (c == '-')
                    || (c == '.')

-- | 'IRIAuth' parser.
parseAuth :: A.Parser (Maybe IRIAuth)
parseAuth = A.option Nothing (A.string "//" *> (Just <$> parseIRIAuth))
    where parseIRIAuth = IRIAuth <$> parseUser
                                 <*> parseHost
                                 <*> parsePort

-- | 'IRIAuth' user parser.
parseUser :: A.Parser (Maybe T.Text)
parseUser = A.option Nothing (Just <$> (A.takeWhile1 isUser <* A.char '@'))
    where isUser c = isIRI c && (c /= '@')

-- | 'IRIAuth' host parser.
parseHost :: A.Parser T.Text
parseHost = A.takeWhile1 isHost
    where isHost c = isIRI c && (c /= '/') && (c /= ':')

-- | 'IRIAuth' port parser.
parsePort :: A.Parser (Maybe T.Text)
parsePort = A.option Nothing (Just <$> (A.char ':' *> A.takeWhile1 isDigit))

-- | 'IRI' path parser.
parsePath :: A.Parser T.Text
parsePath = A.option "" (A.char '/' *> A.takeWhile1 isPath)
    where isPath c = isIRI c && (c /= '?') && (c /= '#')

-- | 'IRI' query parser.
parseQuery :: A.Parser (Maybe T.Text)
parseQuery = A.option Nothing (Just <$> (A.char '?' *> A.takeWhile1 isQuery))
    where isQuery c = isIRI c && (c/= '#')

-- | 'IRI' fragment parser.
parseFragment :: A.Parser (Maybe T.Text)
parseFragment = A.option Nothing (Just <$> (A.char '#' *> A.takeWhile1 isIRI))

-- | Parser for graph labels, i.e. either an escaped 'IRI' or the empty string.
parseGraphLabel :: A.Parser (Maybe IRI)
parseGraphLabel = A.option Nothing (Just <$> parseEscapedIRI)

-- | 'Subject' parser.
parseSubject :: A.Parser Subject
parseSubject = do
    c <- A.anyChar
    case c of '<' -> IRISubject <$> (parseIRI <* A.char '>')
              '_' -> BlankSubject <$> (A.char ':' *> parseBlankNodeLabel)
              _   -> fail "parseSubject: must be blank node or IRI."

-- | 'Predicate' parser.
parsePredicate :: A.Parser Predicate
parsePredicate = Predicate <$> parseEscapedIRI

-- | 'Object' parser.
parseObject :: A.Parser Object
parseObject = do
    c <- A.anyChar
    case c of '<' -> IRIObject <$> (parseIRI <* A.char '>')
              '_' -> BlankObject <$> (A.char ':' *> parseBlankNodeLabel)
              _   -> LiteralObject <$> parseLiteralBody

-- | Parse an escaped 'IRI', i.e. an IRI enclosed in angle brackets.
parseEscapedIRI :: A.Parser IRI
parseEscapedIRI = A.char '<' *> parseIRI <* A.char '>'

-- | Parse a blank node label.
parseBlankNodeLabel :: A.Parser BlankNode
parseBlankNodeLabel = BlankNode <$> (A.takeWhile1 isLabel >>= check)
    where check t
            | isHead (T.head t) && isTail (T.last t) = pure t
            | otherwise                              = fail "parseBlankNode"
          isLabel  = not . isSpace
          isHead c = isLabel c
                  && (c /= '-')
                  && (c /= '.')
          isTail c = isLabel c
                  && (c /= '.')

-- | Parse a blank node label, with the preceeding @_:@.
parseBlankNode :: A.Parser BlankNode
parseBlankNode = A.string "_:" *> parseBlankNodeLabel

-- | Like 'parseLiteral', but without the leading double quote.
parseLiteralBody :: A.Parser Literal
parseLiteralBody = Literal <$> escString <*> valType
    where valType     = valIRIType <|> valLangType <|> pure LiteralUntyped
          valIRIType  = LiteralIRIType <$> (A.string "^^" *> parseEscapedIRI)
          valLangType = LiteralLangType <$> (A.char '@' *> A.takeWhile1 isLang)
          isLang c    = isAlphaNum c || (c == '-')
          escString = unescapeAll <$> A.scan False machine
          machine False '\\' = Just True
          machine False '"'  = Nothing
          machine False _    = Just False
          machine True _     = Just False
          unescapeAll = T.concat . unescapeFrag . T.splitOn "\\"
          unescapeFrag []     = []
          unescapeFrag (f:fs) = case T.uncons f of
                Nothing        -> f : unescapeFrag fs
                (Just (e, f')) -> T.singleton (unescape e) : f' : unescapeFrag fs
          unescape 't' = '\t'
          unescape 'b' = '\b'
          unescape 'n' = '\n'
          unescape 'r' = '\r'
          unescape 'f' = '\f'
          unescape c   = c

-- | Parse an RDF 'Literal', including the 'LiteralType' if present.
parseLiteral :: A.Parser Literal
parseLiteral = A.char '"' *> parseLiteralBody

-- | Parse an unescaped untyped RDF 'Literal'.
parseUnescapedLiteral :: A.Parser Literal
parseUnescapedLiteral = Literal <$> A.takeText <*> pure LiteralUntyped

-- | Make implementations for 'fromString' from a 'A.Parser'.
fromStringParser :: A.Parser a    -- ^ The literal parser.
                 -> String        -- ^ The literal type name for error messages.
                 -> (String -> a) -- ^ The 'fromString' implementation.
fromStringParser p n s = let t = T.pack s
                             r = A.parseOnly p t
                         in case r of (Left e)  -> error $ mconcat
                                                      [ "Invalid "
                                                      , n
                                                      , " literal ("
                                                      , s
                                                      , ") "
                                                      , e
                                                      ]
                                      (Right x) -> x

-- | This instance uses 'parseIRI' and calls 'error' if the literal is invalid.
--   It is not clear exactly when 'fromString' is evaluated so this error is
--   difficult to explictly catch. This can be solved by ensuring that your
--   'IRI' literals are eagerly evaluated so any malformed literals can be
--   caught immediately. It would be nicer if this happened at compile time.
instance IsString IRI where
    fromString = fromStringParser parseIRI "IRI"

-- | This instance uses 'parseLiteral' and calls 'error' if the literal is
--   invalid. It is not clear exactly when 'fromString' is evaluated so this
--   error is difficult to explictly catch. This can be solved by ensuring that
--   your 'Literal' literals are eagerly evaluated so any malformed literals can
--   be caught immediately. It would be nicer if this happened at compile time.
instance IsString Literal where
    fromString = fromStringParser p "Literal"
        where p = parseLiteral <|> parseUnescapedLiteral

-- | This instance uses 'parseBlankNode' and calls 'error' if the literal is
--   invalid. It is not clear exactly when 'fromString' is evaluated so this
--   error is difficult to explictly catch. This can be solved by ensuring that
--   your 'BlankNode' literals are eagerly evaluated so any malformed literals
--   can be caught immediately. It would be nicer if this happened at compile
--   time.
instance IsString BlankNode  where
    fromString = fromStringParser parseBlankNode "BlankNode"

-- | This instance uses 'parseSubject' and calls 'error' if the literal
--   is invalid. It is not clear exactly when 'fromString' is evaluated so this
--   error is difficult to explictly catch. This can be solved by ensuring that
--   your 'Subject' literals are eagerly evaluated so any malformed literals can
--   be caught immediately. It would be nicer if this happened at compile time.
instance IsString Subject where
    fromString = fromStringParser parseSubject "Subject"

-- | This instance uses 'parsePredicate' and calls 'error' if the literal is
--   invalid. It is not clear exactly when 'fromString' is evaluated so this
--   error is difficult to explictly catch. This can be solved by ensuring that
--   your 'Predicate' literals are eagerly evaluated so any malformed literals
--   can be caught immediately. It would be nicer if this happened at compile
--   time.
instance IsString Predicate where
    fromString = fromStringParser parsePredicate "Predicate"

-- | This instance uses 'parseObject' and calls 'error' if the literal is
--   invalid. It is not clear exactly when 'fromString' is evaluated so this
--   error is difficult to explictly catch. This can be solved by ensuring that
--   your 'Object' literals are eagerly evaluated so any malformed literals can
--   be caught immediately. It would be nicer if this happened at compile time.
instance IsString Object where
    fromString = fromStringParser p "Object"
        where p = parseObject <|> (LiteralObject <$> parseUnescapedLiteral)
