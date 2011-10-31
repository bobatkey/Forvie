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
    where g = do
            eof <- liftIO $ IO.hIsEOF handle
            if eof then return Nothing
                   else liftIO (IO.hGetChar handle) >>= return . Just

charsTo :: MonadIO m => IO.Handle -> Reader Char m ()
charsTo handle = mapM_ (liftIO . IO.hPutChar handle)

linesOf :: MonadIO m => IO.Handle -> Stream m T.Text
linesOf handle = generate g
    where g = do
            line <- liftIO $ TIO.hGetLine handle
            return (if T.null line then Nothing else Just line)

linesTo :: MonadIO m => IO.Handle -> Reader T.Text m ()
linesTo handle = mapM_ (liftIO . TIO.hPutStrLn handle)

chunksOf :: MonadIO m => IO.Handle -> Int -> Stream m B.ByteString
chunksOf handle bufferSize = generate g
    where g = do
            s <- liftIO $ B.hGet handle bufferSize
            if B.null s then return Nothing
                        else return (Just s)

chunksTo :: MonadIO m => IO.Handle -> Reader B.ByteString m ()
chunksTo handle = mapM_ (liftIO . B.hPut handle)
