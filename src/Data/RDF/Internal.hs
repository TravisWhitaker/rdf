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

{-# LANGUAGE BangPatterns
           , DeriveGeneric
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
parseScheme = T.toLower <$> (T.cons <$> A.letter <*> A.takeWhile1 isScheme)
    where isScheme c = (isAlphaNum c)
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
    where isUser c = (isIRI c) && (c /= '@')

-- | 'IRIAuth' host parser.
parseHost :: A.Parser T.Text
parseHost = A.takeWhile1 isHost
    where isHost c = (isIRI c) && (c /= '/') && (c /= ':')

-- | 'IRIAuth' port parser.
parsePort :: A.Parser (Maybe T.Text)
parsePort = A.option Nothing (Just <$> (A.char ':' *> A.takeWhile1 isDigit))

-- | 'IRI' path parser.
parsePath :: A.Parser T.Text
parsePath = A.option "" ((A.char '/') *> A.takeWhile1 isPath)
    where isPath c = (isIRI c) && (c /= '?') && (c /= '#')

-- | 'IRI' query parser.
parseQuery :: A.Parser (Maybe T.Text)
parseQuery = A.option Nothing (Just <$> ((A.char '?') *> A.takeWhile1 isQuery))
    where isQuery c = (isIRI c) && (c/= '#')

-- | 'IRI' fragment parser.
parseFragment :: A.Parser (Maybe T.Text)
parseFragment = A.option Nothing (Just <$> ((A.char '#') *> A.takeWhile1 isIRI))

-- | Parser for graph labels, i.e. either an escaped 'IRI' or the empty string.
parseGraphLabel :: A.Parser (Maybe IRI)
parseGraphLabel = A.option Nothing (Just <$> parseEscapedIRI)

-- | 'Subject' parser.
parseSubject :: A.Parser Subject
parseSubject = (IRISubject <$> parseEscapedIRI)
           <|> (BlankSubject <$> parseBlankNode)

-- | 'Predicate' parser.
parsePredicate :: A.Parser Predicate
parsePredicate = Predicate <$> parseEscapedIRI

-- | 'Object' parser.
parseObject :: A.Parser Object
parseObject = (IRIObject <$> parseEscapedIRI)
          <|> (BlankObject <$> parseBlankNode)
          <|> (LiteralObject <$> parseLiteral)

-- | Parse an escaped 'IRI', i.e. an IRI enclosed in angle brackets.
parseEscapedIRI :: A.Parser IRI
parseEscapedIRI = A.char '<' *> parseIRI <* A.char '>'

-- | Parse a blank node label.
parseBlankNode :: A.Parser BlankNode
parseBlankNode = BlankNode <$> (A.string "_:" *> label)
    where label       = T.cons <$> labelHead <*> (A.option T.empty labelBody)
          labelHead   = A.satisfy isHead
          labelBody   = (A.takeWhile isLabel) >>= checkEnd
          checkEnd t
                | T.null t        = pure t
                | T.last t /= '.' = pure t
                | otherwise       = fail "label must not end with '.'"
          isLabel     = not . isSpace
          isHead c    = (isLabel c)
                     && (c /= '-')
                     && (c /= '.')
          isTail c    = (isLabel c)
                     && (c /= '.')

-- | Parse an RDF 'Literal', including the 'LiteralType' if present.
parseLiteral :: A.Parser Literal
parseLiteral = Literal <$> litVal <*> valType
    where litVal      = A.char '"' *> escString
          valType     = valIRIType <|> valLangType <|> pure LiteralUntyped
          valIRIType  = LiteralIRIType <$> (A.string "^^" *> parseEscapedIRI)
          valLangType = LiteralLangType <$> (A.char '@' *> A.takeWhile1 isLang)
          isLang c    = (isAlphaNum c) || (c == '-')
          escString   = do
                c <- A.anyChar
                case c of '"'  -> pure T.empty
                          '\\' -> T.cons <$> (res <$> A.anyChar) <*> escString
                          _    -> T.cons c <$> escString
          res 't' = '\t'
          res 'b' = '\b'
          res 'n' = '\n'
          res 'r' = '\r'
          res 'f' = '\f'
          res c   = c

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
    fromString = fromStringParser (parseLiteral <|> parseUnescapedLiteral)
                                  "Literal"

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
    fromString = fromStringParser parseObject "Object"
