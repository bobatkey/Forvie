-- |
-- Module         :  Control.StreamProcessor.ByteString
-- Copyright      :  Robert Atkey 2011
-- License        :  BSD3
--
-- Maintainer     :  Robert.Atkey@cis.strath.ac.uk
-- Stability      :  experimental
-- Portability    :  unknown
--
-- Stream readers for reading binary data from a source of 'Word8'
-- values.

module Control.StreamProcessor.Binary where

import Data.Functor
import Control.StreamProcessor
import Data.Word

-- | Attempt to read a single 'Word8' value from an input stream of
-- 'Word8' values. If no value is available (because end-of-stream has
-- been reached), then an error is reported.
readWord8 :: UnexpectedEOSError e => SR e Word8 Word8
readWord8 = inputValue

-- | Attempt to read a big endian 32-bit word from the head of a
-- stream of 'Word8' values. If no value is available (because
-- end-of-stream is reached before four bytes are read), then an error
-- is reported.
readWord32BE :: UnexpectedEOSError e => SR e Word8 Word32
readWord32BE =
    do b1 <- fromIntegral <$> readWord8
       b2 <- fromIntegral <$> readWord8
       b3 <- fromIntegral <$> readWord8
       b4 <- fromIntegral <$> readWord8
       return (b1 * 0x1000000 + b2 * 0x10000 + b3 * 0x100 + b4)

-- | Attempt to read a little endian 32-bit word from the head of a
-- stream of 'Word8' values. If no value is available (because
-- end-of-stream is reached before four bytes are read), then an error
-- is reported.
readWord32LE :: UnexpectedEOSError e => SR e Word8 Word32
readWord32LE =
    do b1 <- fromIntegral <$> readWord8
       b2 <- fromIntegral <$> readWord8
       b3 <- fromIntegral <$> readWord8
       b4 <- fromIntegral <$> readWord8
       return (b4 * 0x1000000 + b3 * 0x10000 + b2 * 0x100 + b1)

-- | Attempt to read a big endian 16-bit word from the head of a
-- stream of 'Word8' values. If no value is available (because
-- end-of-stream is reached before four bytes are read), then an error
-- is reported.
readWord16BE :: UnexpectedEOSError e => SR e Word8 Word16
readWord16BE =
    do b1 <- fromIntegral <$> readWord8
       b2 <- fromIntegral <$> readWord8
       return (b1 * 0x100 + b2)

