{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.ByteString.Base16.Lazy
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : bos@mailrank.com
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
-- The only instance in which it is non-lazy is if an odd-length chunk
-- ends with a byte that is valid base16.
--
-- Examples:
--
-- > decode "666f6f"  == ("foo", "")
-- > decode "66quux"  == ("f", "quux")
-- > decode "666quux" == ("f", "6quux")
decode :: ByteString -> (ByteString, ByteString)
decode = foldrChunks go (Empty, Empty)
  where go c ~(y,z)
           | len == 0 = (chunk h y, z)
           | len == 1 && isHex (B.unsafeHead t) =
               case z of
                 Chunk a as | isHex (B.unsafeHead a)
                   -> let (q,_) = B16.decode (t `B.snoc` B.unsafeHead a)
                      in (chunk h (chunk q y), chunk (B.unsafeTail a) as)
                 _ -> (chunk h y, chunk t z)
           | otherwise = (chunk h y, chunk t z)
            where (h,t) = B16.decode c
                  len = B.length t

isHex :: Word8 -> Bool
isHex w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
