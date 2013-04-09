-- |
-- Module:     Network.FastIRC.Raw
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Network.FastIRC.Raw
    ( -- * Raw protocol
      Message(..),
      -- * Parsing
      ircLines,
      messageParser,
      parseMessage
    )
    where

import Control.Applicative
import Control.DeepSeq
import Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import Data.Data


-- | A raw IRC message consists of a command, a list of arguments and an
-- optional prefix.

data Message =
    Message {
      msgPrefix :: Maybe ByteString,  -- ^ Prefix (source of the message).
      msgCmd    :: ByteString,        -- ^ Protocol command.
      msgArgs   :: [ByteString]       -- ^ Command arguments.
    }
    deriving (Data, Eq, Ord, Read, Show, Typeable)

instance NFData Message where
    rnf (Message pfx cmd args) =
        pfx `seq` cmd `seq` args `seq` ()


-- | Parser for a raw protocol message.

messageParser :: Parser Message
messageParser =
    Message
    <$> (prefix <* space <|> pure Nothing)
    <*> command
    <*> many arg

    where
    spaceChars = "\0\r\n "
    lfChars    = "\0\r\n"

    isLF     = inClass lfChars
    isSpace  = inClass spaceChars
    notSpace = notInClass spaceChars

    space = satisfy isSpace *> skipWhile isSpace
    token = takeWhile1 notSpace

    prefix = word8 58 *> (Just <$> token)

    command = token

    arg = space *> (lastArg <|> regArg)
        where
        lastArg = word8 58 *> takeTill isLF
        regArg  = token


-- | Parse the given message.

parseMessage :: ByteString -> Maybe Message
parseMessage =
    either (const Nothing) Just .
    parseOnly messageParser
