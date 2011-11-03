-- |
-- Module         : Data.MonadicStream.IO
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Monadic Streams and IO.

module Data.MonadicStream.IO
    ( charsOf
    , charsTo
    , linesOf
    , linesTo
    , stringLinesOf
    , stringLinesTo
    , byteStringLinesOf
    , byteStringLinesTo
    , chunksOf
    , chunksTo
    )
    where

import           Prelude hiding (mapM_)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import           Control.Monad.Trans (MonadIO (..))
import           Data.MonadicStream
import qualified System.IO as IO

charsOf :: MonadIO m => IO.Handle -> Stream m Char
charsOf handle = generate g
    where
      {-# INLINE g #-}
      g = do
            eof <- liftIO $ IO.hIsEOF handle
            if eof then return Nothing
                   else liftIO (IO.hGetChar handle) >>= return . Just
{-# INLINE charsOf #-}

charsTo :: MonadIO m => IO.Handle -> Reader Char m ()
charsTo handle = mapM_ (liftIO . IO.hPutChar handle)
{-# INLINE charsTo #-}

linesOf :: MonadIO m => IO.Handle -> Stream m T.Text
linesOf handle = generate g
    where
      {-# INLINE g #-}
      g = do
        line <- liftIO $ TIO.hGetLine handle
        return (if T.null line then Nothing else Just line)
{-# INLINE linesOf #-}

linesTo :: MonadIO m => IO.Handle -> Reader T.Text m ()
linesTo handle = mapM_ (liftIO . TIO.hPutStrLn handle)
{-# INLINE linesTo #-}

stringLinesOf :: MonadIO m => IO.Handle -> Stream m String
stringLinesOf handle = generate g
    where
      {-# INLINE g #-}
      g = do
        line <- liftIO $ IO.hGetLine handle
        return (if null line then Nothing else Just line)
{-# INLINE stringLinesOf #-}

stringLinesTo :: MonadIO m => IO.Handle -> Reader String m ()
stringLinesTo handle = mapM_ (liftIO . IO.hPutStrLn handle)
{-# INLINE stringLinesTo #-}

chunksOf :: MonadIO m => IO.Handle -> Int -> Stream m B.ByteString
chunksOf handle bufferSize = generate g
    where g = do
            s <- liftIO $ B.hGet handle bufferSize
            if B.null s then return Nothing
                        else return (Just s)

chunksTo :: MonadIO m => IO.Handle -> Reader B.ByteString m ()
chunksTo handle = mapM_ (liftIO . B.hPut handle)
{-# INLINE chunksTo #-}

byteStringLinesOf :: MonadIO m => IO.Handle -> Stream m B.ByteString
byteStringLinesOf handle = generate g
    where
      {-# INLINE g #-}
      g = do line <- liftIO $ B.hGetLine handle
             if B.null line then return Nothing
                                else return (Just line)
{-# INLINE byteStringLinesOf #-}

byteStringLinesTo :: MonadIO m => IO.Handle -> Reader B.ByteString m ()
byteStringLinesTo handle = mapM_ (liftIO . B.hPutStrLn handle)
{-# INLINE byteStringLinesTo #-}
