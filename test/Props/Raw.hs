module Props.Raw where

import qualified Data.ByteString.Char8 as Bc
import Network.FastIRC.Raw
import Props.Types


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
