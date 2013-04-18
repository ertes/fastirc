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
      ircMessages,
      parseMessage,
      parseMessage',
      printMessage
    )
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.ByteString.Lazy.Builder as Bb
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Proxy
import Data.ByteString (ByteString)
import Data.Data
import Data.Monoid
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
ircLines n _ = runIdentityP (loop 0 mempty B.empty)
    where
    isSep :: Word8 -> Bool
    isSep c = c == 10 || c == 13

    loop i s ds
        | i >= n           = respond (final s) >> skipRest ds >>= loop 0 mempty
        | B.null ds       = request () >>= loop i s
        | not (B.null sfx)  = respond (final s') >> skipLF sfx >>= loop 0 mempty
        | otherwise       = loop (i + B.length pfx) s' sfx
        where
        (pfx, sfx) = B.break isSep ds
        s'         = s <> Bb.byteString pfx
        final      = B.take n . Bl.toStrict . Bb.toLazyByteString

    skipWith :: (Word8 -> Bool) -> ByteString -> Pipe (IdentityP p) ByteString ByteString m ByteString
    skipWith f ds
        | B.null sfx = request () >>= skipWith f
        | otherwise  = return sfx
        where
        sfx = B.dropWhile f ds

    skipLF   = skipWith isSep
    skipRest = skipWith (not . isSep) >=> skipLF

{-# SPECIALIZE ircLines :: (Monad m) => Int -> () -> Pipe ProxyFast ByteString ByteString m r #-}


-- | Turn a stream of IRC protocol lines into a stream of IRC messages.
-- Invalid messages are silently ignored.
--
-- Note:  You cannot parse a raw packet stream with this proxy.  It must
-- have been processed by 'ircLines' first.

ircMessages ::
    (Monad m, Proxy p)
    => () -> Pipe p ByteString Message m r
ircMessages = runIdentityK loop
    where
    loop u =
        parseMessage' <$> request () >>=
        maybe (loop u) (respond >=> loop)

{-# SPECIALIZE ircMessages :: (Monad m) => () -> Pipe ProxyFast ByteString Message m r #-}


-- | Parse the given IRC message.  May not contain line delimiters or
-- the NUL character.

parseMessage :: ByteString -> Maybe Message
parseMessage s = do
    guard (B.notElem 10 s)
    guard (B.notElem 13 s)
    parseMessage' s


-- | Like 'parseMessage', but does not check for line delimiters.

parseMessage' :: ByteString -> Maybe Message
parseMessage' =
    evalStateT $ do
        StateT $ \s -> do
            guard (B.notElem 0 s)
            Just ((), s)
        Message <$> prefix <*> command <*> args

    where
    prefix =
        StateT $ \s ->
            case B.uncons s of
              Just (58, s') -> do
                  let (pfx, s'') = B.break (== 32) s'
                  guard (not (B.null pfx))
                  Just (Just pfx, B.dropWhile (== 32) s'')
              _ -> Just (Nothing, s)

    command =
        StateT $ \s ->
            case B.uncons s of
              Just (c, _) | c /= 32 && c /= 58 ->
                  let (pfx, s') = B.break (== 32) s
                  in Just (pfx, B.dropWhile (== 32) s')
              _ -> Nothing

    args = StateT ((\as -> Just (as, B.empty)) . loop)
        where
        loop s
            | B.null s = []
            | Just (58, rest) <- B.uncons s = [rest]
            | otherwise = arg : loop (B.dropWhile (== 32) s')
            where
            (arg, s') = B.break (== 32) s


-- | Turn the given message into a protocol line including CRLF.

printMessage :: Message -> ByteString
printMessage msg =
    Bl.toStrict . Bb.toLazyByteString $
        maybe mempty pfx (msgPrefix msg) <>
        Bb.byteString (msgCmd msg) <>
        args (msgArgs msg) <>
        Bb.word8 13 <>
        Bb.word8 10

    where
    args [] = mempty
    args [a] =
        Bb.word8 32 <>
        (if (B.null a || B.head a == 58 || B.elem 32 a)
           then Bb.word8 58
           else mempty) <>
        Bb.byteString a
    args (a:as) = Bb.word8 32 <> Bb.byteString a <> args as

    pfx p =
        Bb.word8 58 <>
        Bb.byteString p <>
        Bb.word8 32
