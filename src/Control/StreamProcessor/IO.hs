module Control.StreamProcessor.IO where

import           Control.StreamProcessor
import qualified Data.ByteString as B
import           System.IO

-- FIXME: handle IO errors somehow

onHandle :: Handle -> Int -> SR e B.ByteString a -> IO (Either e a)
onHandle h bufSize = go
    where
      go (Read k)      = do s <- B.hGet h bufSize
                            if B.null s then go (k Nothing)
                                        else go (k (Just s))
      go (ReadError e) = return $ Left e
      go (Yield a)     = return $ Right a
{-# INLINE onHandle #-}

onFile :: FilePath -> Int -> SR e B.ByteString a -> IO (Either e a)
onFile filename bufSize sr =
    do h <- openFile filename ReadMode
       result <- onHandle h bufSize sr
       hClose h
       return result

--------------------------------------------------------------------------------
onHandles :: Handle -> Handle -> SP e B.ByteString B.ByteString -> IO (Maybe e)
onHandles input output = go
    where
      go (Get k)   = do s <- B.hGet input 8192
                        if B.null s then go (k Nothing)
                                    else go (k (Just s))
      go (Put b k) = do B.hPut output b; go k
      go (Error e) = do return (Just e)
      go EOS       = do return Nothing

onFiles :: FilePath -> SP e B.ByteString B.ByteString -> IO (Maybe e)
onFiles filename sp =
    do h <- openFile filename ReadMode
       result <- onHandles h stdout sp
       hClose h
       return result
