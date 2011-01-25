{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.ByteString.Base16
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD
-- Maintainer  : bos@mailrank.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and efficient encoding and decoding of base16-encoded strings.

module Data.ByteString.Base16
    (
      encode
    , decode
    ) where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.ByteString.Char8 (empty)
import Data.ByteString.Internal (ByteString(..), createAndTrim', unsafeCreate)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

digits :: ByteString
digits = "0123456789abcdef"
{-# NOINLINE digits #-}

encode :: ByteString -> ByteString
encode (PS sfp soff slen)
    | slen > maxBound `div` 2 = error "Data.ByteString.Base16.encode: input too large"
    | otherwise = unsafeCreate (slen*2) $ \dptr ->
                    withForeignPtr sfp $ \sptr ->
                      enc (sptr `plusPtr` soff) dptr
 where
  enc sptr = go sptr where
    e = sptr `plusPtr` slen
    go s d | s == e = return ()
           | otherwise = do
      x <- peek8 s
      poke d . unsafeIndex digits $ x `shiftR` 4
      poke (d `plusPtr` 1) . unsafeIndex digits $ x .&. 0xf
      go (s `plusPtr` 1) (d `plusPtr` 2)

decode :: ByteString -> (ByteString, ByteString)
decode (PS sfp soff slen) =
  unsafePerformIO . createAndTrim' (slen `div` 2) $ \dptr ->
      withForeignPtr sfp $ \sptr ->
        dec (sptr `plusPtr` soff) dptr
 where
  dec sptr = go sptr where
    e = sptr `plusPtr` if odd slen then slen - 1 else slen
    go s d | s == e = let len = e `minusPtr` sptr
                      in return (0, len `div` 2, ps sfp (soff+len) (slen-len))
           | otherwise = do
      let hex w
              | w >= 48 && w <= 57  = w - 48
              | w >= 97 && w <= 102 = w - 97 + 10
              | w >= 65 && w <= 70  = w - 65 + 10
              | otherwise           = 0xff
      hi <- hex `fmap` peek8 s
      lo <- hex `fmap` peek8 (s `plusPtr` 1)
      if lo == 0xff || hi == 0xff
        then let len = s `minusPtr` sptr
             in return (0, len `div` 2, ps sfp (soff+len) (slen-len))
        else do
          poke d . fromIntegral $ lo + (hi `shiftL` 4)
          go (s `plusPtr` 2) (d `plusPtr` 1)

peek8 :: Ptr Word8 -> IO Int
peek8 p = fromIntegral `fmap` peek p

ps :: ForeignPtr Word8 -> Int -> Int -> ByteString
ps fp off len
    | len <= 0 = empty
    | otherwise = PS fp off len
