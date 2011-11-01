-- |
-- Module         : Data.MonadicStream.Binary
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Monadic Streams and binary stuff.

module Data.MonadicStream.Binary
    ( ofByteString
    , ofLazyByteString
    , unpack
    , word8
    )
    where

import           Prelude hiding (concatMap, head)
import           Data.MonadicStream
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word

ofByteString :: Monad m => B.ByteString -> Stream m Word8
ofByteString = ofList . B.unpack

ofLazyByteString :: Monad m => BL.ByteString -> Stream m Word8
ofLazyByteString = ofList . BL.unpack

-- FIXME: probably faster ways of writing this
unpack :: Monad m => Processor B.ByteString m Word8
unpack = concatMap B.unpack

word8 :: Monad m => Reader Word8 m Word8
word8 = do w <- head
           case w of
             Nothing -> fail "Unexpected EOS" -- FIXME: proper error
             Just w  -> return w
