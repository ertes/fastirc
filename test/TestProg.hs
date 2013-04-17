module Main where

import Control.Proxy
import Network.FastIRC.Raw


main :: IO ()
main =
    runProxy (fromListS ["abc", "def\n", "ghi\n"] >-> ircLines 5 >-> printD)
