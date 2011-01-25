{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module         :  Control.StreamProcessor
-- Copyright      :  Robert Atkey 2011
-- License        :  BSD3
--
-- Maintainer     :  Robert.Atkey@cis.strath.ac.uk
-- Stability      :  experimental
-- Portability    :  unknown
--
-- Types for describing stream processors and stream readers.
--
-- A stream processor is a function that transforms an input stream of
-- values to an output stream of values. This module provides a type
-- that encodes such functions as a state machine. Normally in
-- Haskell, a stream processor would be represented as a function of
-- type @[a] -> [b]@, but the encoding in this module provides several
-- advantages:
--
--    * The state of the stream processor is explicitly represented,
--    so it is possible to pause and store the state of processors
--    mid-stream. This allows for the input to the stream processor to
--    be provided piece-meal. For example, from a network socket or
--    file handle. See the "Control.StreamProcessor.IO" module.
--
--    * Allow for the possibility of errors in stream processing to be
--    reported, without making use of the impure 'error' function.
--
-- Stream processors are an instance of 'Control.Category.Category',
-- allowing them to be composed in sequence. See the documentation of
-- the 'SP' type below for more details.
--
-- This module also provides an encoding stream reader functions,
-- which read from a stream and produce a single output. As normal
-- Haskell functions, these would have type @[a] -> b@. The encoding
-- in this module provides the same advantages as for stream
-- processors. See the documentation of the 'SR' type below for more
-- details.
--
-- The representation of stream readers and stream processors is based
-- on the paper Representing ... FIXME by Hank, Neil and Dirk.

module Control.StreamProcessor
    (-- * Examples of Use
      -- $example
      
    -- * Stream Processors
      SP (..)
    -- ** Primitive Stream Processors
    , mapSP
    , filterSP
      
    -- * Stream Readers
    , SR (..)
    -- ** Primitive Stream Readers
    , input
    , readerError
    , UnexpectedEOSError (..)
    , inputValue
    , gather
      
    -- ** Running Stream Readers
    , onList

    -- * Interfacing Processors and Readers
    , (>>|)
    , iterate
    , iterateUntil
    )
    where

import Prelude hiding ((.), id, iterate)
import Control.Category
import Control.Applicative

-- $example
-- FIXME: do an example

-- | This type encodes stream processors that transform a stream of
-- input values to a stream of output values. At any point, a stream
-- processor has four options, represented by the four constructors of
-- this type:
--
--   * Demand input with 'Get'. If there is more input then the
--   provided function will be called with 'Just a', otherwise
--   'Nothing' to indicate that the input stream has finished. The
--   stream processor should then evolve to a new state
--
--   * Report an error with 'Error'. Under the default composition
--   operator this will halt the processing of a pipeline and report
--   the error to the caller.
--
--   * Output a value with 'Put'. The stream processor provides a
--   value to be output and a new stream processor to continue
--   processing.
--
--   * Report end of stream with 'EOS'. This need not necessarily be
--   at the same time as the end of the input stream.
--
-- Concrete stream processors are typically written as a collection of
-- mutual recursive functions, each function encoding a state of the
-- stream processor.
--
-- Stream processors can also be constructed by composition: 'SP' is
-- an instance of 'Control.Category.Category'.
--
-- FIXME: talk about inadequacies in this encoding
data SP e a b = Get (Maybe a -> SP e a b)
              | Error e
              | Put b (SP e a b)
              | EOS

idSP :: SP e a a
idSP = Get $ \input -> case input of
                         Nothing -> EOS
                         Just a  -> Put a idSP
{-# INLINE idSP #-}
                         
-- FIXME: consider other stream processor composition operators
composeSP :: SP e a b -> SP e b c -> SP e a c
Get f    `composeSP` sp2     = Get $ \a -> f a `composeSP` sp2
Error e  `composeSP` sp2     = Error e
Put b k1 `composeSP` Get k2  = k1 `composeSP` k2 (Just b)
EOS      `composeSP` Get k2  = EOS `composeSP` k2 Nothing
sp1      `composeSP` Error e = Error e
sp1      `composeSP` Put c k = Put c (sp1 `composeSP` k)
sp1      `composeSP` EOS     = EOS
{-# INLINE composeSP #-}


instance Category (SP e) where
    id = idSP
    sp2 . sp1 = composeSP sp1 sp2

-- | Take each value from the input and apply the given function to
-- transform it. When the input reports End-of-Stream, this stream
-- processor reports End-of-Stream. This is actually a synchronous
-- stream processor, in that input and output are performed in strict
-- lock-step, but the 'SP' type is too loose to express this.
mapSP :: (a -> b) -> SP e a b
mapSP f = Get $ \a -> case a of
                        Nothing -> EOS
                        Just a  -> Put (f a) (mapSP f)

-- | Filter the input stream according to the given
-- function. 'Nothing's are discarded and 'Just's are kept. When the
-- input reports End-of-Stream, this stream processor immediately
-- reports End-of-Stream.
filterSP :: (a -> Maybe b) -> SP e a b
filterSP f = Get $ \a -> case a of
                           Nothing -> EOS
                           Just a  -> case f a of
                                        Nothing -> filterSP f
                                        Just b  -> Put b (filterSP f)

--------------------------------------------------------------------------------
run :: SP String a b -> [a] -> [b]
run (Get k) []     = run (k Nothing) []
run (Get k) (a:as) = run (k (Just a)) as
run (Error e) _    = error $ "error: " ++ e -- FIXME: yes, I know
run (Put b k) as   = b : run k as
run EOS       _    = []

{------------------------------------------------------------------------------}
-- | Stream readers encode functions that consume a (prefix of) a
-- stream of values to produce a single value, or an error. Stream
-- readers are encoded as state machines. At every state, a stream
-- reader has three options, corresponding to the three constructors.
--
--   * Demand input with the 'Read' constructor. If there is no more
--   input then 'Nothing' is passed to the function.
--
--   * Report an error with 'ReadError'.
--
--   * Yield the final output value with 'Yield'.
--
-- There are two options for writing stream readers. One can write
-- then as a collection of mutual recursive functions, in the style of
-- a state machine. This is similar to the usual style for stream
-- processors (see the documentation for the 'SP') type. One can also
-- make use of the fact that 'SR' is a 'Monad' instance and a
-- 'Control.Applicative.Applicative' instance and write stream readers
-- in direct style.
--
-- A potential disadvantage of writing stream readers
-- in direct style is that this can become inefficient due to the
-- (unavoidable) implementation of the bind operator. Wrapping the
-- 'SR' type in the continuation monad transformer eliminates this
-- inefficiency.
data SR e a b = Read (Maybe a -> SR e a b)
              | ReadError e
              | Yield b

-- FIXME: would using a right kan extension speed this up?
instance Monad (SR e a) where
    return = Yield
    Read k      >>= f = Read $ \a -> k a >>= f
    ReadError e >>= f = ReadError e
    Yield b     >>= f = f b

instance Functor (SR e a) where
    fmap f (Yield a)     = Yield (f a)
    fmap f (ReadError e) = ReadError e
    fmap f (Read k)      = Read $ \x -> fmap f $ k x

instance Applicative (SR e a) where
    pure = return
    Yield f     <*> a = f <$> a
    ReadError e <*> _ = ReadError e
    Read k      <*> a = Read (\i -> k i <*> a)

--------------------------------------------------------------------------------
infixr 0 >>|

-- | Compose a stream processor with a stream reader to produce a
-- stream reader. The resulting stream reader effectively
-- pre-processes the input according to the stream processor, and then
-- passes this into the stream reader.
(>>|) :: SP e a b -> SR e b c -> SR e a c
Get f    >>| sr          = Read $ \a -> f a >>| sr
Error e  >>| sr          = ReadError e
Put b k1 >>| Read k2     = k1 >>| k2 (Just b)
EOS      >>| Read k2     = EOS >>| k2 Nothing
sp       >>| ReadError e = ReadError e
sp       >>| Yield c     = Yield c
{-# INLINE (>>|) #-}

{------------------------------------------------------------------------------}
-- | Demands a single item of input. Yields 'Nothing' if there is no
-- more input to be had.
input :: SR e a (Maybe a)
input = Read Yield

-- | Immediately reports the given error. No input is demanded.
readerError :: e -> SR e a b
readerError e = ReadError e

-- | Type class for representing the unexpected presence of the
-- End-of-Stream event.
class UnexpectedEOSError e where
    unexpectedEOS :: e

instance UnexpectedEOSError String where
    unexpectedEOS = "Unexpected End Of Stream"

inputValue :: UnexpectedEOSError e => SR e a a
inputValue = Read handle
    where handle Nothing  = ReadError unexpectedEOS
          handle (Just a) = Yield a

{------------------------------------------------------------------------------}
gather :: SR e a [a]
gather = go []
    where
      go l = Read (input l)
      input l Nothing  = Yield (reverse l)
      input l (Just a) = go (a:l)

{------------------------------------------------------------------------------}
iterate :: SR e a b -> SP e a b
iterate sr = go sr
    where
      go (Read k)      = Get $ go . k
      go (ReadError e) = Error e
      go (Yield b)     = Put b $ go sr

iterateUntil :: SR e a (Maybe b) -> SP e a b
iterateUntil sr = go sr
    where
      go (Read k)         = Get $ go . k
      go (ReadError e)    = Error e
      go (Yield Nothing)  = EOS
      go (Yield (Just b)) = Put b (go sr)

once :: SR e a b -> SP e a b
once (Read k)      = Get $ once . k
once (ReadError e) = Error e
once (Yield b)     = Put b EOS

{------------------------------------------------------------------------------}
onList :: SR e a b -> [a] -> Either e b
onList sr l = go l sr
    where
      go []     (Read k)      = go [] (k Nothing)
      go (a:as) (Read k)      = go as (k (Just a))
      go as     (ReadError e) = Left e
      go as     (Yield b)     = Right b
