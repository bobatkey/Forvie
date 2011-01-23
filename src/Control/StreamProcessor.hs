{-# LANGUAGE TypeSynonymInstances #-}

module Control.StreamProcessor
    ( -- * Stream Processors
      SP (..)
    , mapSP
    , filterSP
      
      -- * Stream Readers
    , SR (..)
    , (>>|)
    , UnexpectedEOSError (..)
    , readInput
      
    , gather
    , iterate
    , iterateUntil
    , onList
    )
    where

import Prelude hiding ((.), id, iterate)
import Control.Category
import Control.Applicative
import System.IO

data SP e a b = Get (Maybe a -> SP e a b)
              | Error e
              | Put b (SP e a b)
              | EOS

idSP :: SP e a a
idSP = Get $ \input -> case input of
                         Nothing -> EOS
                         Just a  -> Put a idSP
                         
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

-- this is actually a synchronous stream processor
mapSP :: (a -> b) -> SP e a b
mapSP f = Get $ \a -> case a of
                        Nothing -> EOS
                        Just a  -> Put (f a) (mapSP f)

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

(>>|) :: SP e a b -> SR e b c -> SR e a c
Get f    >>| sr          = Read $ \a -> f a >>| sr
Error e  >>| sr          = ReadError e
Put b k1 >>| Read k2     = k1 >>| k2 (Just b)
EOS      >>| Read k2     = EOS >>| k2 Nothing
sp       >>| ReadError e = ReadError e
sp       >>| Yield c     = Yield c
{-# INLINE (>>|) #-}

{------------------------------------------------------------------------------}
class UnexpectedEOSError e where
    unexpectedEOS :: e

instance UnexpectedEOSError String where
    unexpectedEOS = "Unexpected End Of Stream"

readInput :: UnexpectedEOSError e => SR e a a
readInput = Read handle
    where handle Nothing  = ReadError unexpectedEOS
          handle (Just a) = Yield a

readError :: e -> SR e a b
readError e = ReadError e

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

{------------------------------------------------------------------------------}
onList :: SR e a b -> [a] -> Either e b
onList sr l = go l sr
    where
      go []     (Read k)      = go [] (k Nothing)
      go (a:as) (Read k)      = go as (k (Just a))
      go as     (ReadError e) = Left e
      go as     (Yield b)     = Right b
