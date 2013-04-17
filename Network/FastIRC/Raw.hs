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

import qualified Data.ByteString as B
import Control.Applicative
import Control.DeepSeq
import Control.Monad.Writer
import Control.Proxy
import Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import Data.Data
import Data.Word


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


-- | Splits a raw bytestring stream into a stream of IRC message lines.

ircLines ::
    forall m p r. (Monad m, Proxy p)
    => Int  -- ^ Maximum line length (not including separators).
    -> ()
    -> Pipe p ByteString ByteString m r
ircLines n _ = runIdentityP (loop B.empty B.empty)
    where
    isSep :: Word8 -> Bool
    isSep c = c == 10 || c == 13

    loop s ds
        | B.length s >= n  = respond (B.take n s) >> skipRest ds >>= loop B.empty
        | B.null ds       = request () >>= loop s
        | not (B.null sfx)  = respond (B.take n s') >> skipLF sfx >>= loop B.empty
        | otherwise       = loop s' sfx
        where
        (pfx, sfx) = B.break isSep ds
        s'         = B.append s pfx

    skipWith :: (Word8 -> Bool) -> ByteString -> Pipe (IdentityP p) ByteString ByteString m ByteString
    skipWith f ds
        | B.null sfx = request () >>= skipWith f
        | otherwise  = return sfx
        where
        sfx = B.dropWhile f ds

    skipLF   = skipWith isSep
    skipRest = skipWith (not . isSep) >=> skipLF

{-# SPECIALIZE ircLines :: (Monad m) => Int -> () -> Pipe ProxyFast ByteString ByteString m r #-}


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
