-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Benchmark for the fastirc package.

module Main where

import qualified Crypto.Random.AESCtr as Rnd
import qualified Data.ByteString as B
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Writer
import Control.Proxy ((>->), fromListS, runProxy, toListD)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.List
import Network.FastIRC.Raw
import System.Random


testSession :: [ByteString]
testSession =
    take 100000 .
    unfoldr (Just . runState gen) .
    maybe (error "Unable to set up AESCtr RNG") id .
    Rnd.make $
    "kaidiechohcieyahlaeviquiepeiveeshiediujahseichiwahtuehengeenaish"

    where
    gen =
        state (randomR (1, 1500)) >>=
        state . Rnd.genRandomBytes


main :: IO ()
main = do
    let ts = testSession
    print . sum . map B.length $ ts
    ts `deepseq` defaultMain $
        [ bgroup "raw" (raw ts) ]

    where
    raw ts = [parseArgs, parseCmd, parseFull, parseSpc,
              split]
        where
        parseArgs = bench "parse_args" (nf parseMessage "COMMAND ARG ARG ARG :LAST ARG")
        parseCmd  = bench "parse_cmd"  (nf parseMessage "COMMAND")
        parseFull = bench "parse_full" (nf parseMessage ":PREFIX COMMAND ARG ARG ARG :LAST ARG")
        parseSpc  = bench "parse_spc"  (nf parseMessage "C                                 ")

        split = bench "split" $
                nf (\ts' ->
                        execWriter . runProxy $
                        fromListS ts' >-> ircLines 512 >-> toListD) ts
