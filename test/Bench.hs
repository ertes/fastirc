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
import qualified Data.ByteString.Char8 as Bc
import qualified Data.ByteString.Lazy as Bl
import Blaze.ByteString.Builder
import Control.Applicative
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Proxy ((>->), fromListS, runProxy, useD)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Monoid
import Data.List
import Network.FastIRC.Raw
import System.Random


testMessage :: Message
testMessage =
    Message (Just "very!long@prefix")
            "PRIVMSG"
            ["#somechannel", "this is a not so long message, but it does it"]


testSession :: [ByteString]
testSession =
    take (10*2048) .
    cycle .
    take 2048 .
    map (Bl.toStrict . Bl.take 512) .
    iterate (Bl.drop 512) .
    Bl.fromChunks .
    unfoldr (Just . runState gen) .
    maybe (error "Unable to set up AESCtr RNG") id .
    Rnd.make $
    "kaidiechohcieyahlaeviquiepeiveeshiediujahseichiwahtuehengeenaish"

    where
    gen =
        (\pfx cmd args larg ->
             Bc.unwords $ [B.cons 58 pfx, cmd] ++ args ++ [B.cons 58 larg `Bc.append` "\r\n"])
        <$> (genR (10, 30) >>= \l -> B.pack <$> replicateM l (genR (0x41, 0x5A)))
        <*> (genR (3, 10) >>= \l -> B.pack <$> replicateM l (genR (0x41, 0x5A)))
        <*> (genR (3, 10) >>= \n ->
                 replicateM n $
                 genR (2, 20) >>= \l ->
                 B.pack <$> replicateM l (genR (0x61, 0x7A)))
        <*> (genR (10, 400) >>= \l -> B.pack <$> replicateM l (genR (32, 127)))

    genR :: (Random a) => (a, a) -> State Rnd.AESRNG a
    genR = state . randomR


main :: IO ()
main = do
    let tm = testMessage
        ts = testSession
    defaultMain $
        tm `deepseq` ts `deepseq`
        [ bgroup "raw" (raw tm ts) ]

    where
    raw tm ts =
        [parseArgs, parseCmd, parseFull, parseSpc,
         printMsg,
         streamParse, streamSplit]

        where
        parseArgs = bench "parse_args" (nf parseMessage "COMMAND ARG ARG ARG :LAST ARG")
        parseCmd  = bench "parse_cmd"  (nf parseMessage "COMMAND")
        parseFull = bench "parse_full" (nf parseMessage ":PREFIX COMMAND ARG ARG ARG :LAST ARG")
        parseSpc  = bench "parse_spc"  (nf parseMessage "C                                 ")

        printMsg =
            bench "print" $
            nf (toByteString .
                mconcat .
                replicate 100 .
                fromMessage) tm

        streamParse =
            bench "stream_parse_10M" $
            nf (\ts' ->
                    runIdentity . runProxy $
                    fromListS ts' >->
                    ircLines 512 >->
                    ircMessages >->
                    useD (\ln -> ln `seq` return ())) ts

        streamSplit =
            bench "stream_split_10M" $
            nf (\ts' ->
                    runIdentity . runProxy $
                    fromListS ts' >->
                    ircLines 512 >->
                    useD (\ln -> ln `seq` return ())) ts
