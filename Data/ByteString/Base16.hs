{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      : Data.ByteString.Base16
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : Herbert Valerio Riedel <hvr@gnu.org>,
--               Mikhail Glushenkov <mikhail.glushenkov@gmail.com>,
--               Emily Pillmore <emilypi@cohomolo.gy>
-- Stability   : stable
-- Portability : non-portable
--
-- RFC 4648-compliant Base16 (Hexadecimal) encoding for 'ByteString' values.
--
module Data.ByteString.Base16
( encode
, decode
, decodeLenient
) where

import Data.ByteString (empty)
import Data.ByteString.Base16.Internal
import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr

import GHC.ForeignPtr
#if __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import GHC.IO (unsafeDupablePerformIO)
#endif


-- | Encode a 'ByteString' value in base16 (i.e. hexadecimal).
-- Encoded values will always have a length that is a multiple of 2.
-- See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
-- === __Examples__:
--
-- > encode "foo"  == "666f6f"
--
encode :: ByteString -> ByteString
encode (PS sfp soff slen)
    | slen > maxBound `div` 2 =
      error "Data.ByteString.Base16.encode: input too long"
    | otherwise = unsafeCreate (slen * 2) $ \dptr ->
        withForeignPtr sfp $ \sptr ->
          encodeLoop dptr
          (sptr `plusPtr` soff)
          (sptr `plusPtr` (soff + slen))

-- | Decode a base16-encoded 'ByteString' value.
--
-- If errors are encountered during the decoding process,
-- then they will be returned in the @Left@ clause of the
-- coproduct. See: <https://tools.ietf.org/html/rfc4648#section-8 RFC-4648 section 8>
--
--
-- === __Examples__:
--
-- > decode "666f6f"  == Right "foo"
-- > decode "66quux"  == Left "invalid character at offset: 2"
-- > decode "666quux" == Left "invalid character at offset: 3"
--
-- @since 1.0.0.0
--
decode :: ByteString -> Either String ByteString
decode (PS sfp soff slen)
    | slen == 0 = Right empty
    | r /= 0 = Left "invalid bytestring size"
    | otherwise = unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes q
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr ->
          decodeLoop dfp dptr
          (plusPtr sptr soff)
          (plusPtr sptr (soff + slen))
  where
    !q = slen `quot` 2
    !r = slen `rem` 2

-- | Decode a Base16-encoded 'ByteString' value leniently, using a
-- strategy that never fails.
--
-- /N.B./: this is not RFC 4648-compliant
--
-- === __Examples__:
--
-- > decodeLenient "666f6f"  == "foo"
-- > decodeLenient "66quuxx" == "f"
-- > decodeLenient "666quux" == "f"
-- > decodeLenient "666fquu" -- "fo"
--
-- @since 1.0.0.0
--
decodeLenient :: ByteString -> ByteString
decodeLenient (PS !sfp !soff !slen)
    | slen == 0 = empty
    | otherwise = unsafeDupablePerformIO $ do
      dfp <- mallocPlainForeignPtrBytes (q * 2)
      withForeignPtr dfp $ \dptr ->
        withForeignPtr sfp $ \sptr ->
          lenientLoop
            dfp
            dptr
            (plusPtr sptr soff)
            (plusPtr sptr (soff + slen))
  where
    !q = slen `quot` 2
