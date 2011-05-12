{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances, FlexibleContexts #-}

-- |
-- Module           :  Data.DFA
-- Copyright        :  Robert Atkey 2011
-- License          :  BSD3
--
-- Maintainer       :  Robert.Atkey@cis.strath.ac.uk
-- Stability        :  experimental
-- Portability      :  unknown
--
-- Representation, simulation and construction of Deterministic Finite
-- Automata (DFAs).
--
-- This module uses the 'TotalMap' type from Data.RangeSet to
-- represent the transition functions for each state. This can provide
-- a compact representation even when the alphabet of the DFA is
-- large, e.g. all Unicode codepoints.
--
-- Construction of DFA is usually done by using the 'makeDFA'
-- function. This takes values of types that can be treated as
-- deterministic finite state machines and constructs a concrete
-- finite state machine with the same behaviour. The algorithm used is
-- an abstraction of the one presented by Owens et al in "Regular
-- expression derivatives re-examined" (FIXME: proper reference).

module Data.DFA
    ( -- * Representation
      DFA (..)
      
      -- * Simulation
    , runDFA
    , TransitionResult (..)
    , transition
      
      -- * Construction
    , FiniteStateAcceptor (..)
    , makeDFA
    )
    where

import           Prelude hiding (lookup)
import           Control.Arrow (first)
import           Data.Maybe (isJust)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Array (Array, array, (!))
import           Data.RangeSet
import           Data.BooleanAlgebra (one)
import           Data.List  (find)
import "mtl"     Control.Monad.State

{------------------------------------------------------------------------------}
class (Ord r, Enum (Alphabet r), Ord (Alphabet r), Bounded (Alphabet r))
    => FiniteStateAcceptor r where
    type Alphabet r  :: *
    type Result r    :: *
    advance          :: Alphabet r -> r -> r
    isAcceptingState :: r -> Maybe (Result r)
    classes          :: r -> Partition (Alphabet r)

-- FIXME: Make a newtype constructor for this.
instance (FiniteStateAcceptor r, Ord a) => FiniteStateAcceptor [(r,a)] where
    type Alphabet [(r,a)] = Alphabet r
    type Result [(r,a)] = a
    advance c        = map (first $ advance c)
    isAcceptingState = fmap snd . find (isJust . isAcceptingState . fst)
    classes          = foldl andClasses (fromSet one) . map (classes . fst)

data DFAWithState a b = DFAWithState (DFA a b) Int

toDFAWithState :: DFA a b -> DFAWithState a b
toDFAWithState dfa = DFAWithState dfa 0

{-
instance Ord a => FiniteStateAcceptor (DFAWithState a b) where
    type Alphabet (DFAWithState a b) = a
    type Result (DFAWithState a b)   = b
    advance c (DFAWithState dfa q) = undefined -- FIXME
    isAcceptingState (DFAWithState dfa q) = undefined
    classes (DFAWithState dfa q) = undefined -- FIXME: need something to get the domain of a total map?
-}  
-- FIXME: can we do DFA minimisation with this setup?

{------------------------------------------------------------------------------}
-- DFA construction
data ConstructorState re
    = CS { csStates      :: M.Map re Int
         , csNextState   :: Int
         , csTransitions :: IM.IntMap (TotalMap (Alphabet re) Int)
         , csFinalReachingStates :: IS.IntSet
         , csFinalStates :: IM.IntMap (Result re)
         }

type ConstructorM re a = State (ConstructorState re) a

haveVisited :: Ord re => re -> ConstructorM re (Maybe Int)
haveVisited r = get >>= return . M.lookup r . csStates

setTransitions :: FiniteStateAcceptor re => Int -> TotalMap (Alphabet re) Int -> ConstructorM re ()
setTransitions src map =
    modify $ \s -> s { csTransitions = IM.insert src map (csTransitions s) }

-- FIXME: could compress all error states into one?
newState :: FiniteStateAcceptor re => re -> ConstructorM re Int
newState r = do
  CS states next trans finalReaching final <- get
  let states' = M.insert r next states
      next'   = next + 1
      finalReaching' = case isAcceptingState r of
                         Nothing -> finalReaching
                         Just _  -> IS.insert next finalReaching
      final'  = case isAcceptingState r of
                  Nothing  -> final
                  Just res -> IM.insert next res final
  put (CS states' next' trans finalReaching' final')
  return next

-- we are exploring the state space by DFS, so we could return 'accReachable' as
-- we go. Add everything that is 'accReachable' to some set. Take the complement
-- of the set to get the error state set.

explore :: FiniteStateAcceptor re => re -> ConstructorM re Int
explore q = do
  s <- newState q
  t <- makeTotalMapM (classes q)
       $ \c -> do let q' = advance c q
                  visited <- haveVisited q'
                  s'      <- case visited of Nothing -> explore q'; Just s -> return s
                  frs     <- gets csFinalReachingStates
                  when (IS.member s' frs) $ modify $ \cs -> cs { csFinalReachingStates = IS.insert s (csFinalReachingStates cs) }
                  return s'
  setTransitions s t
  return s

-- | The 'DFA' type has two parameters, the type of input tokens 'a'
-- and the type of output annotations for final states 'b'.
--
-- The states of a DFA are represented as 'Int' values in the range
-- '[0..n]', where 'n' is the number of states of the
-- automaton.
data DFA a b =
    DFA { -- | Transition functions of the DFA, indexed by state number.
          transitions :: Array Int (TotalMap a Int)
          -- | The set of error states. Transitions from states in this set will always lead back to this set, and never to an accepting state.
        , errorStates :: IS.IntSet
          -- | The set of accepting states, with final values.
        , finalStates :: IM.IntMap b
        }
    deriving Show

instance Functor (DFA a) where
    fmap f dfa =
        dfa { finalStates = fmap f (finalStates dfa) }

makeDFA :: FiniteStateAcceptor re => re -> DFA (Alphabet re) (Result re)
makeDFA r = DFA transArray error final
    where
      init = CS M.empty 0 IM.empty IS.empty IM.empty

      CS states next trans finalReaching final = execState (explore r) init

      error = IS.fromList [ i | i <- [0..next-1], not (IS.member i finalReaching) ]

      transArray = array (0,next-1) (IM.assocs trans)

{------------------------------------------------------------------------------}
{- Running DFAs -}
data TransitionResult a
    = Accepting a Int
    | Error
    | Change Int
      deriving (Eq, Ord, Show)

transition :: Ord a => DFA a b -> Int -> a -> TransitionResult b
transition dfa state c = result
    where
      DFA transitions errorStates acceptingStates = dfa
      
      newState = lookup (transitions ! state) c
      
      result = if IS.member newState errorStates then Error
               else case IM.lookup newState acceptingStates of
                      Nothing -> Change newState
                      Just a  -> Accepting a newState

-- | 
runDFA :: Ord a =>
          DFA a b -> -- A concrete representation of a deterministic finite automaton
          [a] ->     -- A list of input tokens from the DFA's alphabet. Must be finite.
          Maybe b    -- Acceptance state of the DFA at the end of the input list.
runDFA dfa = aux 0
    where
      DFA transitions _ final = dfa

      aux s []     = IM.lookup s final
      aux s (c:cs) = aux (lookup (transitions ! s) c) cs
