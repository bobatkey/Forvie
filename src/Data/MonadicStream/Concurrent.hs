-- |
-- Module         : Data.MonadicStream.Concurrent
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Monadic Streams and Concurrency.

module Data.MonadicStream.Concurrent
    ( forkProcessor
    , forkStream
    , forkReader
    , fromChan
    , toChan
    )
    where

import Data.MonadicStream
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan)
import Control.Monad.Trans (MonadIO (..))

fromChan :: MonadIO m =>
            Chan a
         -> Stream m a
fromChan ch = generate (do a <- liftIO (readChan ch); return (Just a))

toChan :: MonadIO m =>
          Chan a
       -> Reader a m ()
toChan ch = consumeBy g ()
    where g () Nothing  = return (Right ())
          g () (Just a) = do liftIO $ writeChan ch a
                             return (Left ())

-- How to implement fair merging of two streams?
-- Could have a thing that does something using timeouts


forkProcessor :: Processor a IO b
              -> IO (Reader a IO (), Stream IO b)
forkProcessor processor = do
  chInput  <- newChan
  chOutput <- newChan
  _        <- forkIO (fromChan chInput |>> processor |>| toChan chOutput)
  return (toChan chInput, fromChan chOutput)

-- could just return a new processor?
-- this would probably be pointless because we would be forcing it to be synchronous

forkStream :: Stream IO a
           -> IO (Stream IO a)
forkStream stream = do
  ch <- newChan
  _  <- forkIO (stream |>| toChan ch)
  return (fromChan ch)

forkReader :: Reader a IO ()
           -> IO (Reader a IO ())
forkReader reader = do
  ch <- newChan
  _  <- forkIO (fromChan ch |>| reader)
  return (toChan ch)

-- do we need something that allows us to send a request to a service,
-- with some extra data the service does not care about, and get back
-- a response?
