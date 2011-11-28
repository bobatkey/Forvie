{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, FlexibleContexts, DeriveFunctor, GADTs, RankNTypes #-}

module Language.Forvie.Parsing.ParseMonad
    where

import Control.Monad.ST
import Data.STRef
import Data.IORef
import Language.Forvie.Parsing.Parser

import Data.TypedMap (Show1 (..))

--------------------------------------------------------------------------------
data Knot f a = In (f (Knot f) a)

--------------------------------------------------------------------------------
-- A way of representing parse results, with stored position information
data PosKnot f a = InPos Int Int (f (PosKnot f) a)

instance Show2 f => Show1 (PosKnot f) where
    show1 (InPos _ _ x) = show2 x

instance Show2 f => Show (PosKnot f a) where
    show = show1

--------------------------------------------------------------------------------
-- Abstraction over monads that have a notion of updatable reference cell
-- Problem: this means that this is the only way to generate “ParseStackMonad” instances
class Monad m => RefMonad m where
    type Ref m :: * -> *
    newRef    :: a -> m (Ref m a)
    modifyRef :: Ref m a -> (a -> a) -> m ()
    readRef   :: Ref m a -> m a

instance RefMonad IO where
    type Ref IO = IORef
    newRef    = newIORef
    modifyRef = modifyIORef
    readRef   = readIORef

instance RefMonad (ST s) where
    type Ref (ST s) = STRef s
    newRef    = newSTRef
    modifyRef = modifySTRef
    readRef   = readSTRef

instance (ParseResultsMonad f m, RefMonad m) => ParseStackMonad nt tok f m where
    data StackNode nt tok f m a = SN (Ref m (Int, nt a, [WaitingItem nt tok f m a]))
    newStackNode i nt       = do v <- newRef (i, nt, []); return (SN v)
    addToStackNode a (SN v) = modifyRef v (\(i,nt,l) -> (i,nt,a:l))
    readStackNode (SN v)    = do (_,_,l) <- readRef v; return l
    getPositionAndNT (SN v) = do (i,nt,l) <- readRef v; return (i,nt)

--------------------------------------------------------------------------------
-- Ambiguity rejection, generic over the way that waiting sets are stored
data AmbiguityCheckResult f a where
    UnambiguousResult :: a -> AmbiguityCheckResult f a
    AmbiguityDetected :: Int -> Int -> f (PosKnot f) b -> f (PosKnot f) b -> AmbiguityCheckResult f a
    -- and also parse failures

instance Functor (AmbiguityCheckResult f) where
    fmap f (UnambiguousResult a)       = UnambiguousResult (f a)
    fmap f (AmbiguityDetected i j x y) = AmbiguityDetected i j x y

instance (Show2 f, Show a) => Show (AmbiguityCheckResult f a) where
    show (UnambiguousResult a)       = "UnambiguousResult " ++ show a
    show (AmbiguityDetected i j x y) = "(AmbiguityDetected between " ++ show i ++ " and " ++ show j ++ ": " ++ show2 x ++ ")"

newtype AmbiguityCheckMonad f m a =
    ACM { runACM :: m (AmbiguityCheckResult f a) }

instance Monad m => Monad (AmbiguityCheckMonad f m) where
    return a = ACM $ return (UnambiguousResult a)
    ACM c >>= f = ACM $ do
      r <- c
      case r of
        UnambiguousResult a       -> runACM (f a)
        AmbiguityDetected i j x y -> return (AmbiguityDetected i j x y)

instance RefMonad m => RefMonad (AmbiguityCheckMonad f m) where
    type Ref (AmbiguityCheckMonad f m) = Ref m
    newRef a      = ACM $ newRef a >>= return . UnambiguousResult
    modifyRef v f = ACM $ modifyRef v f >>= return . UnambiguousResult
    readRef v     = ACM $ readRef v >>= return . UnambiguousResult

instance Monad m => ParseResultsMonad f (AmbiguityCheckMonad f m) where
    type ResultNode f (AmbiguityCheckMonad f m) = PosKnot f
    newResult i j x           = return (InPos i j x)
    addResult (InPos i j x) y = ACM $ return (AmbiguityDetected i j x y)

----------------------------------------
-- Specialised versions
type AmbiguityCheckMonadIO f a = AmbiguityCheckMonad f IO a

-- FIXME: call this disallowAmbiguity? (and below?)
runACMIO :: AmbiguityCheckMonadIO f a -> IO (AmbiguityCheckResult f a)
runACMIO = runACM

type AmbiguityCheckMonadST s f a = AmbiguityCheckMonad f (ST s) a

runACMST :: (forall s. AmbiguityCheckMonadST s f a) -> AmbiguityCheckResult f a
runACMST c = runST (runACM c)

------------------------------------------------------------------------------
-- FIXME: do an FGL-based generation of the graph, suitable for
-- getting graphviz to generate pretty SPPF diagrams.

------------------------------------------------------------------------------
-- ST based: building actual imperative graphs of the shared packed
-- parse forests. What ought this do on parse errors? Should probably
-- layer on an error monad.
data STRN f s a = STRN (STRef s [f (STRN f s) a])

instance ParseResultsMonad f (ST s) where
    type ResultNode f (ST s) = STRN f s
    newResult i j x      = do v <- newSTRef [x]; return (STRN v)
    addResult (STRN v) x = modifySTRef v (x:)

--------------------------------------------------------------------------------
-- IO based

-- This demonstrates that the parser does a lot of unnecessary work,
-- and that tail call optimisation is desirable
{-
data K a b = K a

instance Show1 (f (K ())) => ParseResultsMonad f IO where
    type ResultNode f IO = K ()
    newResult i j x = do putStrLn $ "New result from " ++ show i ++ " to " ++ show j ++ ": " ++ show1 x
                         return (K ())
    addResult v x   = return ()
-}

-- FIXME: use IORef
instance ParseResultsMonad f IO where
    type ResultNode f IO = PosKnot f
    newResult i j x      = return (InPos i j x)
    addResult _ x        = error "ambiguity detected" -- FIXME: throw a proper error

-- TODO: purely functional version (without GC), with different ambiguity rejection behaviour
--       add an error reporting whotsit
--       use FGL to store the parse result graph

{------------------------------------------------------------------------------}
-- Purely functional representations: conflicts with RefMonad-based instances

--newtype PMonad nt tok f a =
--    PM (StateT (Waiting nt tok f t) Maybe a)

-- Plan: use Maybe to report ambiguity errors
--       use the normal state monad to do garbage collection
