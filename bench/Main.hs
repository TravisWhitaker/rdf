{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main

import qualified Data.ByteString.Builder as B

import Data.RDF.Types
import Data.RDF.Encode.NQuads
import Data.RDF.Parser.NQuads

import Data.String

import qualified Data.Text as T

import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL

-- | This function is inefficient. Run it outside of the benchmarks.
exampleGraph :: Int -> RDFGraph
exampleGraph n = RDFGraph (Just "http://user@benchmark.graph:8888/?graph") (take n (trips 0))
    where trips i = Triple (sub i) (pred i) (obj i) : trips (i+1)
          sub  = fromString . ("_:" ++) . show
          pred = fromString . ("<http://user@benchmark.graph/succ#" ++) . (++ ">") . show
          obj  = fromString . show

exampleDoc :: Int -> TL.Text
exampleDoc = TL.decodeUtf8 . B.toLazyByteString . encodeRDFGraph . exampleGraph

mkBenchEncodeGraph :: String -> Int -> Benchmark
mkBenchEncodeGraph p n = env (return (exampleGraph n))
                             (\ ~g -> bench (p ++ "/" ++ show n)
                                            (nf (B.toLazyByteString . encodeRDFGraph) g))

mkBenchDecodeGraph :: String -> Int -> Benchmark
mkBenchDecodeGraph p n = env (return (exampleDoc n))
                             (\ ~d -> bench (p ++ "/" ++ show n)
                                            (nf parseNQuads d))

main :: IO ()
main = defaultMain [ bgroup "fine" [ bgroup "encodeGraphFine" $ map (mkBenchEncodeGraph "encodeRDFGraph") [500,1000..100000]
                                   , bgroup "parseGraphsFine" $ map (mkBenchDecodeGraph "parseRDFGraph") [500,1000..100000]
                                   ]
                   , bgroup "coarse" [ bgroup "encodeGraphCoarse" $ map (mkBenchEncodeGraph "encodeRDFGraph") [100000,200000..1000000]
                                     , bgroup "parseGraphsCoarse" $ map (mkBenchDecodeGraph "parseRDFGraph") [100000,200000..1000000]
                                     ]
                   ]
