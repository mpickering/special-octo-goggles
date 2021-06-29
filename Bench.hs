{-# LANGUAGE BangPatterns #-}
module Main (main, l, lInline) where
import qualified Encoding as A
import BenchLib


l = A.encodingToLazyByteString . A.list A.int
lInline = A.encodingToLazyByteString . A.inlineList A.int

main = do
  bench "L" $ nf l [1 :: Int ..1000]
  bench "L-inline" $ nf lInline [1 :: Int ..1000]

