-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Benchmark for the fastirc package.

module Main where

import Criterion.Main
import Network.FastIRC.Raw


main :: IO ()
main =
    defaultMain $
    [ bgroup "Raw" [rawParser] ]

    where
    rawParser = bench "parse cmd" (nf parseMessage "COMMAND")
