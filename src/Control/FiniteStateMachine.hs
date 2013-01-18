{-# LANGUAGE TypeFamilies, FlexibleContexts, TypeOperators #-}

-- |
-- Module         : Control.FiniteStateMachine
-- Copyright      : (C) Robert Atkey 2013
-- License        : BSD3
--
-- Maintainer     : bob.atkey@gmail.com
-- Stability      : experimental
-- Portability    : unknown
--
-- This module contains a type class capturing finite state machines
-- over arbitrary input alphabets. This class has the following
-- features:
--
-- * When reaching an accepting state, a value may be returned. See
-- the 'isAcceptingState' function.
--
-- * Very large alphabets (e.g., unicode) are supported by means of
-- the 'classes' function.

module Control.FiniteStateMachine
    ( -- * Finite State Machines: definition and simulation
      FiniteStateMachine (..)
    , runFSM
      -- * Combinators for 'FiniteStateMachine's
    , (:==>) (..)
    , Literal (..)
    )
    where

import Data.RangeSet (Partition, fromSet, singleton)
import Data.Monoid (mempty)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Foldable (foldMap)
import Data.Functor ((<$>))

-- | Class of types whose inhabitants represent finite state
-- machines. Members of this type class should be /finite/ state. So
-- there can only be a finite number of possible states reachable from
-- 'initState' via 'advance'. FIXME: mention why this doesn't
-- necessarily mean that the 'State fsm' type is an instance of
-- 'Bounded'.
class ( Ord (State fsm)
      , Enum (Alphabet fsm)
      , Ord (Alphabet fsm)
      , Bounded (Alphabet fsm))
    => FiniteStateMachine fsm where

    -- | The type of states of finite state acceptors of this type.
    type State fsm :: *

    -- | The type of symbols recognised by this type of finite state
    -- acceptor.
    type Alphabet fsm :: *

    -- | The type of results returned by this type of finite state
    -- acceptor on acceptance of an input sequence.
    type Result fsm :: *

    -- | The initial state of a given finite state acceptor of this
    -- type.
    initState :: fsm -> State fsm

    -- | State transition function given an input token from the
    -- alphabet of this finite state acceptor.
    advance :: fsm -> Alphabet fsm -> State fsm -> State fsm

    -- | Determine whether the given state is an accepting state of
    -- the finite state acceptor. If so, return the result value for
    -- this accepting state.
    isAcceptingState :: fsm -> State fsm -> Maybe (Result fsm)

    -- | Returns a partition of the alphabet of this finite state
    -- acceptor such that for each equivalence class in the partition,
    -- it is the case that 'advance fsm c1 s == advance fsm c2
    -- s'. FIXME: explain this better.
    classes :: fsm -> State fsm -> Partition (Alphabet fsm)

-- | Really should be fixed length vectors, but I can't be
-- bothered. FIXME: might be easier with the data kinds stuff.
instance FiniteStateMachine fsm => FiniteStateMachine [fsm] where
    type State [fsm]    = [State fsm]
    type Alphabet [fsm] = Alphabet fsm
    type Result [fsm]   = Result fsm

    initState r = map initState r
    advance r c s = map (\(r,s) -> advance r c s) $ zip r s
    isAcceptingState r =
        listToMaybe . mapMaybe (\(r,s) -> isAcceptingState r s) . zip r
    classes r = foldMap (uncurry classes) . zip r

-- | A tuple type intended for attaching result values to existing
-- 'FiniteStateMachine's.
data a :==> b = a :==> b
    deriving (Eq, Ord, Show)

infixr 5 :==>

-- | Attaching results to 'FiniteStateMachine's
instance FiniteStateMachine r => FiniteStateMachine (r :==> a) where
     type State (r :==> a)    = State r
     type Alphabet (r :==> a) = Alphabet r
     type Result (r :==> a)   = a
     initState (r :==> a) = initState r
     advance (r :==> a) c s = advance r c s
     isAcceptingState (r :==> a) s = const a <$> isAcceptingState r s
     classes (r :==> a) s = classes r s

--------------------------------------------------------------------------------
-- | Treat a list of items as a 'FiniteStateMachine'. As a
-- 'FiniteStateMachine', @Literal l@ accepts exactly the list of
-- symbols @l@.
newtype Literal a = Literal [a]

-- | Treat a list of items as a 'FiniteStateMachine'. As a
-- 'FiniteStateMachine', @Literal l@ accepts exactly the list of
-- symbols @l@.
instance (Bounded a, Enum a, Ord a) => FiniteStateMachine (Literal a) where
    type State (Literal a)    = Maybe [a]
    type Alphabet (Literal a) = a
    type Result (Literal a)   = ()

    initState (Literal l) = Just l

    advance _ c Nothing   = Nothing
    advance _ c (Just []) = Nothing
    advance _ c (Just (c':cs))
        | c == c'   = Just cs
        | otherwise = Nothing

    isAcceptingState _ (Just []) = Just ()
    isAcceptingState _ _         = Nothing

    classes _ Nothing      = mempty
    classes _ (Just [])    = mempty
    classes _ (Just (c:_)) = fromSet (singleton c)

--------------------------------------------------------------------------------
-- | Simulate a finite state machine on an input sequence. The input
-- sequence should be finite.
runFSM :: FiniteStateMachine fsm =>
          fsm -- ^ A finite state machine
       -> [Alphabet fsm] -- ^ Input sequence, should be finite
       -> Maybe (Result fsm) -- ^ `Nothing` if the sequence is not
                             -- accepted. 'Just x' if the whole
                             -- sequence is accepted
runFSM fsm input
    = run (initState fsm) input
    where
      run q []     = isAcceptingState fsm q
      run q (x:xs) = run (advance fsm x q) xs
