{-# LANGUAGE BangPatterns, OverloadedStrings #-}

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
import Data.Word (Word8)
import Control.Monad (forM_)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

-- | Encode a string into base16 form.  The result will always be a
-- multiple of 2 bytes in length.
--
-- Example:
--
-- > encode "foo"  == "666f6f"
encode :: ByteString -> ByteString
encode (PS sfp soff slen)
    | slen > maxBound `div` 2 =
        error "Data.ByteString.Base16.encode: input too long"
    | otherwise = unsafeCreate (slen*2) $ \dptr ->
                    withForeignPtr sfp $ \sptr ->
                      enc (sptr `plusPtr` soff) dptr
 where
  enc sptr = go sptr where
    e = sptr `plusPtr` slen
    go s d | s == e = return ()
           | otherwise = do
      x <- peek8 s
      poke d =<< (peek (digits `plusPtr` (x `shiftR` 4)) :: IO Word8)
      poke (d `plusPtr` 1) =<< (peek (digits `plusPtr` (x .&. 0xf)) :: IO Word8)
      go (s `plusPtr` 1) (d `plusPtr` 2)
  digits :: Ptr Word8
  !digits = unsafePerformIO $ do
             ptr <- mallocBytes 16
             forM_ (zip [0..] ("0123456789abcdef"::String)) $ \(i,c) ->
               poke (ptr `plusPtr` i) ((fromIntegral (fromEnum c)) :: Word8)
             return ptr
  {-# NOINLINE digits #-}

-- | Decode a string from base16 form. The first element of the
-- returned tuple contains the decoded data. The second element starts
-- at the first invalid base16 sequence in the original string.
--
-- Examples:
--
-- > decode "666f6f"  == ("foo", "")
-- > decode "66quux"  == ("f", "quux")
-- > decode "666quux" == ("f", "6quux")
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
