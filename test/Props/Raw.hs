module Props.Raw where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Bc
import Control.Monad.Writer
import Control.Proxy
import Network.FastIRC.Raw
import Props.Types


prop_ircLines_sample1 (map getLineToken -> lns) (Positive bl) (Positive n) =
    let msgs    = zipWith B.snoc lns (cycle [10, 13])
        packets = takeWhile (not . B.null) .
                  map (B.take bl) .
                  iterate (B.drop bl) .
                  B.concat $ msgs
        res     = execWriter (runProxy $ fromListS packets >-> ircLines n >-> toListD)
    in printTestCase (show (msgs, packets, res)) $
       res == map (B.take n) lns


prop_parser_nonTok (NonToken msg) =
    maybe True (const False) $
    parseMessage msg


prop_parser_command (ArgToken cmd) =
    parseMessage cmd ==
    Just (Message Nothing cmd [])


prop_parser_args (ArgToken cmd) (map getArgToken . take 10 -> args) =
    printTestCase (show res) $
    res == Just (Message Nothing cmd args)

    where
    msg = Bc.unwords (cmd : args)
    res = parseMessage msg


prop_parser_lastArg (ArgToken cmd) (map getArgToken . take 10 -> args) (LastArgToken larg) =
    printTestCase (show res) $
    res == Just (Message Nothing cmd (args ++ [larg]))

    where
    msg = Bc.unwords (cmd : args ++ [Bc.cons ':' larg])
    res = parseMessage msg


rawTests = $(testGroupGenerator)
