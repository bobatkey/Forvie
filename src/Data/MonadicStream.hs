{-# LANGUAGE BangPatterns #-}

-- |
-- Module         : Data.MonadicStream
-- Copyright      : Robert Atkey 2011
-- License        : BSD3
--
-- Maintainer     : Robert.Atkey@cis.strath.ac.uk
-- Stability      : experimental
-- Portability    : unknown
--
-- Types for describing streams interleaved with monadic effects.

module Data.MonadicStream
    ( Stream (..), StreamStep (..)
    , nil
    , cons
    , append
    , concat
    , generate
    , ofList

    , Reader (..), ReaderStep (..)
    , head
    , foldl
    , foldl'
    , toList

    , printAll
    , mapM_

    , Processor (..), ProcessorStep (..)
    , map
    , mapM
    , filter
    , concatMap
    , concatMapAccum
    , concatMapAccumM
    , ofStream

    , (|>|)
    , (>>|)
    , (>>>)
    )
    where

import           Prelude hiding (head, map, filter, mapM, mapM_, concat, concatMap, foldl)
import qualified Data.List as L
import           Control.Applicative (Applicative (..), liftA)
import           Control.Monad (ap)
import           Control.Monad.Trans (MonadTrans (..), MonadIO (..))

{------------------------------------------------------------------------------}
-- | A stream of elements of type 'a' that commits monadic actions in
-- the monad 'm' in order to produce elements. The stream may have an
-- end.
newtype Stream m a
    = Stream { forceStream :: m (StreamStep m a) }

data StreamStep m a
    = StreamElem a (Stream m a)
    | StreamEnd

nil :: Monad m => Stream m a
nil = Stream $ return StreamEnd

cons :: Monad m => a -> Stream m a -> Stream m a
cons a as = Stream $ return (StreamElem a as)

ofList :: Monad m => [a] -> Stream m a
ofList []     = nil
ofList (a:as) = cons a (ofList as)

append :: Monad m => Stream m a -> Stream m a -> Stream m a
append xs ys = Stream $ do
  xsStep <- forceStream xs
  case xsStep of
    StreamElem x xs' -> forceStream (cons x (xs' `append` ys))
    StreamEnd        -> forceStream ys

concat :: Monad m => [Stream m a] -> Stream m a
concat streams = L.foldl append nil streams

-- scan?
generate :: Monad m => m (Maybe a) -> Stream m a
generate generator =
    Stream $ do
      value <- generator
      case value of
        Nothing -> return StreamEnd
        Just a  -> return (StreamElem a (generate generator))

{------------------------------------------------------------------------------}
-- | `Reader e a b` represents functions that read 'a's before
-- yielding a result of type 'b', while commiting monadic actions in
-- the monad 'm'.
newtype Reader a m b
    = Reader { forceReader :: m (ReaderStep a m b) }

data ReaderStep a m b
    = Read    (Maybe a -> Reader a m b)
    | ReadEnd b

instance Monad m => Monad (Reader a m) where
    return a     = Reader $ return (ReadEnd a)
    reader >>= f = Reader $ do
      readerStep <- forceReader reader
      case readerStep of
        Read k    -> return (Read (\i -> k i >>= f))
        ReadEnd b -> forceReader (f b)

instance MonadTrans (Reader a) where
    lift action =
        Reader $ do result <- action
                    return (ReadEnd result)

instance MonadIO m => MonadIO (Reader a m) where
    liftIO action = lift $ liftIO action

instance Monad m => Applicative (Reader a m) where
    pure  = return
    (<*>) = ap

instance Monad m => Functor (Reader a m) where
    fmap = liftA

head :: Monad m => Reader a m (Maybe a)
head = Reader $ return (Read $ \i -> Reader $ return (ReadEnd i))

toList :: Monad m => Reader a m [a]
toList = loop []
    where
      loop buffer = do
        a <- head
        case a of
          Nothing -> return (reverse buffer)
          Just a  -> loop (a:buffer)

foldl :: Monad m => (b -> a -> b) -> b -> Reader a m b
foldl f b = do
  a <- head
  case a of
    Nothing -> return b
    Just a  -> foldl f (f b a)

foldl' :: Monad m => (b -> a -> b) -> b -> Reader a m b
foldl' f !b = do
  a <- head
  case a of
    Nothing -> return b
    Just a  -> foldl' f (f b a)

-- Does the reader before doing the stream
-- Other options:
-- - Make the stream dictate things
-- - Return left-over streams/readers
(|>|) :: Monad m => Stream m a -> Reader a m b -> m b
stream |>| reader = do
  readerStep <- forceReader reader
  case readerStep of
    Read k ->
        do streamStep <- forceStream stream
           case streamStep of
             StreamElem a stream' -> stream' |>| k (Just a)
             StreamEnd            -> nil     |>| k Nothing
    ReadEnd b ->
        return b

{------------------------------------------------------------------------------}
printAll :: (Show a, MonadIO m) => Reader a m ()
printAll = mapM_ (liftIO . print)

mapM_ :: Monad m => (a -> m ()) -> Reader a m ()
mapM_ action = do
  a <- head
  case a of
    Nothing -> return ()
    Just a  -> lift (action a) >> mapM_ action

{------------------------------------------------------------------------------}
newtype Processor a m b
    = Processor { processorReader :: Reader a m (ProcessorStep a m b) }

data ProcessorStep a m b
    = ProcessorEmit b (Processor a m b)
    | ProcessorEnd

processorEmit :: Monad m => b -> Processor a m b -> Reader a m (ProcessorStep a m b)
processorEmit b processor =
    return (ProcessorEmit b processor)

processorEnd :: Monad m => Reader a m (ProcessorStep a m b)
processorEnd =
    return ProcessorEnd

ofStream :: Monad m => Stream m b -> Processor a m b
ofStream stream =
    Processor $ do
      streamStep <- lift $ forceStream stream
      case streamStep of
        StreamElem b stream' -> processorEmit b (ofStream stream')
        StreamEnd            -> processorEnd

map :: Monad m => (a -> b) -> Processor a m b
map f =
    Processor $ do
      a <- head
      case a of
        Nothing -> processorEnd
        Just a  -> processorEmit (f a) (map f)

filter :: Monad m => (a -> Maybe b) -> Processor a m b
filter f = Processor loop
    where
      loop = do
        a <- head
        case a of
          Nothing -> processorEnd
          Just a  ->
              case f a of
                Nothing -> loop
                Just b  -> processorEmit b (filter f)

mapM :: Monad m => (a -> m b) -> Processor a m b
mapM f =
    Processor $ do
      a <- head
      case a of
        Nothing -> processorEnd
        Just a  -> do b <- lift $ f a
                      processorEmit b (mapM f)

concatMap :: Monad m => (a -> [b]) -> Processor a m b
concatMap f =
    Processor $ do
      a <- head
      case a of
        Nothing -> processorEnd
        Just a  -> processorReader $ loop (f a)
    where
      loop []     = concatMap f
      loop (b:bs) = Processor $ processorEmit b (loop bs)

concatMapAccum :: Monad m =>
                  (s -> a -> (s, [b]))
               -> (s -> [b])
               -> s
               -> Processor a m b
concatMapAccum f eos s =
    Processor $ do
      a <- head
      case a of
        Nothing -> let bs = eos s
                   in processorReader $ loop bs (Processor $ processorEnd)
        Just a  -> let (s', bs) = f s a
                   in processorReader $ loop bs (concatMapAccum f eos s')
    where
      loop []     k = k
      loop (b:bs) k = Processor $ processorEmit b (loop bs k)

concatMapAccumM :: Monad m =>
                   (s -> a -> m (s, [b])) -- ^ Monadic action to run on each input
                -> (s -> m [b])           -- ^ Monadic action to run at the end of the input
                -> s                      -- ^ The initial state
                -> Processor a m b        -- ^ Processor that iterates the step action
concatMapAccumM step eos s =
    Processor $ do
      a <- head
      case a of
        Nothing -> do bs <- lift (eos s)
                      processorReader $ loop bs (Processor processorEnd)
        Just a  -> do (s', bs) <- lift (step s a)
                      processorReader $ loop bs (concatMapAccumM step eos s')
    where
      loop []     k = k
      loop (b:bs) k = Processor $ processorEmit b (loop bs k)

-- | Pre-compose a processor onto a reader. The resulting process is
-- driven by the original reader.
(>>|) :: Monad m => Processor a m b -> Reader b m c -> Reader a m c
processor >>| reader =
    Reader $ do
      readerStep <- forceReader reader
      case readerStep of
        Read k ->
            do processorStep <- forceReader $ processorReader processor
               case processorStep of
                 Read k' ->
                     return (Read $ \input -> Processor (k' input) >>| Reader (return (Read k)))
                 ReadEnd ProcessorEnd ->
                     forceReader (ofStream nil >>| k Nothing)
                 ReadEnd (ProcessorEmit b processor') ->
                     forceReader (processor' >>| k (Just b))
        ReadEnd c ->
            return (ReadEnd c)

-- | Demand-driven composition of processors.
(>>>) :: Monad m => Processor a m b -> Processor b m c -> Processor a m c
processor1 >>> processor2 =
    Processor $ Reader $ do
      readerStep2 <- forceReader $ processorReader processor2
      case readerStep2 of
        Read k2 ->
            do readerStep1 <- forceReader $ processorReader processor1
               case readerStep1 of
                 Read k1 ->
                     return (Read $ \input -> processorReader $ Processor (k1 input) >>> Processor (Reader (return (Read k2))))
                 ReadEnd (ProcessorEmit b processor1') ->
                     forceReader $ processorReader $ processor1' >>> Processor (k2 (Just b))
                 ReadEnd ProcessorEnd ->
                     forceReader $ processorReader $ ofStream nil >>> Processor (k2 Nothing)
        ReadEnd (ProcessorEmit c processor2') ->
            return (ReadEnd (ProcessorEmit c (processor1 >>> processor2')))
        ReadEnd ProcessorEnd ->
            return (ReadEnd ProcessorEnd)
