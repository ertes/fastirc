module Main where

import qualified Data.ByteString.Char8 as Bc
import Network.FastIRC.Raw
import System.Environment


main :: IO ()
main =
    getArgs >>=
    mapM_ (print . parseMessage . Bc.pack)
