module Props.Raw where

import qualified Data.ByteString.Char8 as Bc
import Data.Attoparsec (parseOnly)
import Network.FastIRC.Raw
import Props.Types


prop_parser_nonTok (NonToken msg) =
    either (const True) (const False) $
    parseOnly messageP msg


prop_parser_command (ArgToken cmd) =
    parseOnly messageP cmd ==
    Right (Message Nothing cmd [])


prop_parser_args (ArgToken cmd) (map getArgToken . take 10 -> args) =
    printTestCase (show res) $
    res == Right (Message Nothing cmd args)

    where
    msg = Bc.unwords (cmd : args)
    res = parseOnly messageP msg


prop_parser_lastArg (ArgToken cmd) (map getArgToken . take 10 -> args) (LastArgToken larg) =
    printTestCase (show res) $
    res == Right (Message Nothing cmd (args ++ [larg]))

    where
    msg = Bc.unwords (cmd : args ++ [Bc.cons ':' larg])
    res = parseOnly messageP msg


rawTests = $(testGroupGenerator)
