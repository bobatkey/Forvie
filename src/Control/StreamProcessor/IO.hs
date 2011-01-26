-- |
-- Module         :  Control.StreamProcessor.IO
-- Copyright      :  Robert Atkey 2011
-- License        :  BSD3
--
-- Maintainer     :  Robert.Atkey@cis.strath.ac.uk
-- Stability      :  experimental
-- Portability    :  unknown
--
-- 'IO' actions for running stream processors and readers from
-- "Control.StreamProcessor" on 'Handle's and 'FilePath's.

module Control.StreamProcessor.IO
    (
     -- * Running Stream Processors
      onHandles
    , onFiles
     -- * Running Stream Readers
    , onHandle
    , onFile
    )
    where

import           Control.StreamProcessor
import qualified Data.ByteString as B
import           System.IO

-- FIXME: handle IO errors somehow

-- | Run a stream reader on a 'Handle'. When the stream reader demands
-- input, it is read from the handle, in chunks of the given buffer
-- size.
--
-- The handle must be opened in 'ReadMode' or 'ReadWriteMode'. The
-- handle is /not/ closed when the stream reader finishes. The value
-- returned from the 'IO' action is the value yielded by the reader or
-- the error signaled, as appropriate.
onHandle :: Handle -- ^ Handle to read input from
         -> Int -- ^ Buffer size for reading
         -> SR e B.ByteString a -- ^ Stream reader to direct and process the input
         -> IO (Either e a)
onHandle h bufSize = go
    where
      go (Read k)      = do s <- B.hGet h bufSize
                            if B.null s then go (k Nothing)
                                        else go (k (Just s))
      go (ReadError e) = return $ Left e
      go (Yield a)     = return $ Right a
{-# INLINE onHandle #-}

-- | Run a stream reader on a file. The named file is opened in
-- 'ReadMode' and 'onHandle' is invoked. The opened handle is closed
-- when the stream reader terminates.
onFile :: FilePath -- ^ Path to file to open for reading
       -> Int -- ^ Buffer size for reading the input
       -> SR e B.ByteString a -- ^ Stream reader to direct and process the input
       -> IO (Either e a)
onFile filename bufSize sr =
    do h <- openFile filename ReadMode
       result <- onHandle h bufSize sr
       hClose h
       return result

--------------------------------------------------------------------------------
-- | Run a stream processor on a pair of handles, reading from the
-- first handle and writing to the second handle until the stream
-- processor indicates 'EOS' or reports an 'Error'. The return value
-- is 'Nothing' is no error occured, otherwise the reported error is
-- returned.
--
-- Input is read from the first handle in chunks of the given buffer
-- size. The input handle must be opened in either 'ReadMode' or
-- 'ReadWriteMode'. The output handle must be opened in one of
-- 'WriteMode', 'AppendMode' or 'ReadWriteMode'. The handles are /not/
-- closed.
onHandles :: Handle -- ^ Handle to read input from
          -> Handle -- ^ Handle to write output to
          -> Int    -- ^ Buffer size to use for input
          -> SP e B.ByteString B.ByteString -- ^ The stream processor to transform the data
          -> IO (Maybe e)
onHandles input output bufSize = go
    where
      go (Get k)   = do s <- B.hGet input bufSize
                        if B.null s then go (k Nothing)
                                    else go (k (Just s))
      go (Put b k) = do B.hPut output b; go k
      go (Error e) = do return (Just e)
      go EOS       = do return Nothing

-- | Open the given file for reading (in 'ReadMode') and pass it to
-- 'onHandles' with the given buffer size. The output is sent to
-- 'stdout'. When the stream processor terminates, the handle opened
-- for reading is closed.
onFiles :: FilePath -- ^ The name of the file to be opened for reading
        -> Int -- ^ Buffer size to use
        -> SP e B.ByteString B.ByteString -- ^ The stream processor to transform the data
        -> IO (Maybe e)
onFiles filename bufSize sp =
    do h <- openFile filename ReadMode
       result <- onHandles h stdout bufSize sp
       hClose h
       return result
