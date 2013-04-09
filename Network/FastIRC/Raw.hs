-- |
-- Module:     Network.FastIRC.Raw
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>

module Network.FastIRC.Raw
    ( -- * Raw protocol
      Message(..),
      -- * Parsing
      messageP
    )
    where

--import qualified Data.ByteString as B
import Control.Applicative
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


-- | Parser for a raw protocol message.

messageP :: Parser Message
messageP =
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
