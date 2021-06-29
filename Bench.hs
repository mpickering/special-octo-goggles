{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Encoding as A
import BenchLib


main = do
  bench "L" $ nf (A.encodingToLazyByteString . A.list A.int) [1 :: Int ..1000]
  bench "L-inline" $ nf (A.encodingToLazyByteString . A.inlineList A.int) [1 :: Int ..1000]

