{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- Netstrings are a nicer way to send JSON over a socket, because they
-- account for the possibility of malformed JSON much more cleanly.

-- A netstring is an ASCII-encoded sequence of digits representing a
-- number N, followed by an ASCII colon, followed by N bytes, followed
-- by an ASCII comma. This allows receivers to know how much is needed.

-- Definition: http://cr.yp.to/proto/netstrings.txt

module Netstrings where

import Control.Exception
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS

import Data.Word
import System.IO

import Debug.Trace

data BadNetstring = BadLength | MissingColon (Maybe Word8) | MissingComma (Maybe Word8) deriving Show

instance Exception BadNetstring

-- TODO: Let's make a newtype wrapper for encoded netstrings

toNetstring :: ByteString -> ByteString
toNetstring bytes =
  BS.toLazyByteString $
    BS.stringUtf8 (show (BS.length bytes)) <>
    BS.charUtf8 ':' <>
    BS.lazyByteString bytes <>
    BS.charUtf8 ','

-- >>> toNetstring "hello"
-- "5:hello,"


-- | Read a netstring from a handle
netstringFromHandle :: Handle -> IO ByteString
netstringFromHandle h =
  do l <- len Nothing
     bytes <- BS.hGet h l
     c <- BS.head <$> BS.hGet h 1
     if isComma c then return bytes else throw (MissingComma (Just c))

  where
    len Nothing =
      do x <- BS.head <$> BS.hGet h 1
         if not (isDigit x)
           then throw BadLength
           else len (Just (asDigit x))
    len (Just acc) =
      do x <- BS.head <$> BS.hGet h 1
         if | isColon x -> return acc
            | isDigit x -> len (Just (10 * acc + asDigit x))
            | otherwise -> throw (MissingColon (Just x))

-- | Attempt to split a ByteString into a prefix that is a valid netstring, and an arbitrary suffix
fromNetstring :: ByteString -> (ByteString, ByteString)
fromNetstring input =
  let (lenBytes, rest) = BS.span isDigit input
      len = asLength lenBytes
  in
    case BS.uncons rest of
      Nothing -> throw (MissingColon Nothing)
      Just (c, rest')
        | isColon c ->
            let (content, rest'') = BS.splitAt (fromIntegral len) rest'
            in
              case BS.uncons rest'' of
                Nothing -> throw (MissingComma Nothing)
                Just (b, done) | isComma b -> (content, done)
                               | otherwise -> throw (MissingComma (Just b))
        | otherwise -> throw (MissingColon (Just c))

isDigit :: Word8 -> Bool
isDigit w = w >= 0x30 && w <= 0x39

isColon :: Word8 -> Bool
isColon w = w == 0x3a

isComma :: Word8 -> Bool
isComma w = w == 0x2c

-- | Assumes isDigit of argument
asDigit :: Word8 -> Int
asDigit w = fromIntegral (w - 0x30)

asLength :: ByteString -> Int
asLength len =
  if BS.null len
    then throw BadLength
    else go 0 (map asDigit (BS.unpack len))
  where
    go acc [] = acc
    go acc (d:ds) = go (acc * 10 + d) ds

-- >>> :set -XOverloadedStrings
-- >>> fromNetstring "5:hello,aldskf"
-- ("hello","aldskf")