{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.ByteString.Base16.Lazy
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base16-encoded strings.

module Data.ByteString.Base16.Lazy
    (
      encode
    , decode
    ) where

import Data.Word (Word8)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Internal

-- | Encode a string into base16 form.  The result will always be a
-- multiple of 2 bytes in length.
--
-- Example:
--
-- > encode "foo"  == "666f6f"
encode :: ByteString -> ByteString
encode (Chunk c cs) = Chunk (B16.encode c) (encode cs)
encode Empty        = Empty

-- | Decode a string from base16 form. The first element of the
-- returned tuple contains the decoded data. The second element starts
-- at the first invalid base16 sequence in the original string.
--
-- This function operates as lazily as possible over the input chunks.
--
-- Examples:
--
-- > decode "666f6f"  == ("foo", "")
-- > decode "66quux"  == ("f", "quux")
-- > decode "666quux" == ("f", "6quux")
decode :: ByteString -> (ByteString, ByteString)
decode = go Nothing
  where
      go :: Maybe Word8 -> ByteString -> (ByteString, ByteString)
      go Nothing Empty = (Empty, Empty)
      go (Just w) Empty = (Empty, BL.singleton w)
      go (Just w) (Chunk c z) =
           go Nothing (chunk (B.pack [w, B.unsafeHead c]) (chunk (B.unsafeTail c) z))
      go Nothing (Chunk c z)
           | len == 0 =
                 let ~(res,tail') = go Nothing z
                 in (chunk h res, tail')
           | len == 1 && isHex (B.unsafeHead t) =
                 let ~(res,tail') = go (Just (B.unsafeHead t)) z
                 in (chunk h res, tail')
           | otherwise = (chunk h Empty, chunk t z)
            where (h,t) = B16.decode c
                  len = B.length t

isHex :: Word8 -> Bool
isHex w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
