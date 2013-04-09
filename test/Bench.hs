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
    [ bgroup "raw" raw ]

    where
    raw = [parseArgs, parseCmd, parseFull, parseSpc]
        where
        parseArgs = bench "parse_args" (nf parseMessage "COMMAND ARG ARG ARG :LAST ARG")
        parseCmd  = bench "parse_cmd"  (nf parseMessage "COMMAND")
        parseFull = bench "parse_full" (nf parseMessage ":PREFIX COMMAND ARG ARG ARG :LAST ARG")
        parseSpc  = bench "parse_spc"  (nf parseMessage "C                                 ")
