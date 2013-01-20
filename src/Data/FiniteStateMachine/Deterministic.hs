{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}

-- |
-- Module           :  Data.FiniteStateMachine.Deterministic
-- Copyright        :  (C) Robert Atkey 2013
-- License          :  BSD3
--
-- Maintainer       :  bob.atkey@gmail.com
-- Stability        :  experimental
-- Portability      :  unknown
--
-- Representation, simulation and construction of Deterministic Finite
-- Automata (DFAs).
--
-- This module uses the 'TotalMap' type from "Data.RangeSet" to
-- represent the transition functions for each state. This can provide
-- a compact representation even when the alphabet of the DFA is
-- large, e.g. all Unicode codepoints.
--
-- Construction of DFA is usually done by using the 'makeDFA'
-- function. This takes values of types that can be treated as
-- deterministic finite state machines and constructs a concrete
-- finite state machine with the same behaviour. The algorithm used is
-- an abstraction of the one presented by Owens et al in /Regular
-- expression derivatives re-examined/ (FIXME: proper reference).
--
-- The DFAs represented here have explicit representation of error
-- states. An error state is a state from which an accepting state can
-- never be reached. Explicit representation of error states is needed
-- when using DFAs for gathering longest matches when scanning text to
-- determine lexemes.
--
-- FIXME: write more about the difference between 'DFA's and
-- 'FiniteStateMachine's.

module Data.FiniteStateMachine.Deterministic
    ( -- * Representation
      DFA (..)
      
      -- * Construction
    , makeDFA

      -- * Simulation
    , TransitionResult (..)
    , transition )
    where

import           Data.Foldable (foldMap)
import           Data.Maybe (maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import           Data.Array (Array, array, (!))
import           Data.RangeSet (TotalMap, ($@), domainPartition, makeTotalMapA)
import qualified Control.Monad.State as S
import           Control.Monad.State (modify, gets, execState, join, unless)
import           Data.Functor ((<$>))
import           Data.FiniteStateMachine

-- | The 'DFA' type has two parameters, the type of input tokens 'a'
-- and the type @b@ of output values attached to accepting states. FIXME: rewrite this to mention 'deterministic finite automaton'
--
-- The states of a DFA are represented as 'Int' values in the range
-- '[0..n]', where 'n' is the number of states of the
-- automaton. State '0' is always the initial state.
data DFA alphabet result = DFA
    { -- | Transition functions of the DFA, indexed by state number.
      transitions :: !(Array Int (TotalMap alphabet Int))
    -- | The set of error states. Transitions from states in this set
    -- will always lead back to this set, and never to an accepting
    -- state.
    , errorStates :: !IS.IntSet
    -- | The set of accepting states, with attached values.
    , acceptingStates :: !(IM.IntMap result)
    } deriving (Eq, Show)

-- | DFAs are finite state machines.
instance (Enum alphabet, Bounded alphabet, Ord alphabet) =>
    FiniteStateMachine (DFA alphabet result) where
    data State (DFA alphabet result) =
        DFAState { unDFAState :: Int } deriving (Eq, Ord)
    type Alphabet (DFA alphabet result) = alphabet
    type Result (DFA alphabet result)   = result
    initState dfa = DFAState 0

    advance dfa a q = DFAState $ (transitions dfa ! unDFAState q) $@ a

    isAcceptingState dfa q = IM.lookup (unDFAState q) (acceptingStates dfa)

    classes dfa q = domainPartition (transitions dfa ! unDFAState q)

-- | Transform the result values of a DFA.
instance Functor (DFA alphabet) where
    fmap f dfa =
        dfa { acceptingStates = fmap f (acceptingStates dfa) }

{------------------------------------------------------------------------------}
-- DFA construction
data ConstructionState fsm
    = CS { csVisited     :: !(M.Map (State fsm) Int)
         , csNextState   :: !Int
         , csTransitions :: !(IM.IntMap (TotalMap (Alphabet fsm) Int))
         , csBackEdges   :: !(IM.IntMap [Int])
         , csAccepting   :: !(IM.IntMap (Result fsm))
         }

type ConstructionM re a = S.State (ConstructionState re) a

haveVisited :: Ord (State fsm) =>
               State fsm
            -> ConstructionM fsm (Maybe Int)
haveVisited q = gets (M.lookup q . csVisited)

setTransitions :: FiniteStateMachine fsm =>
                  Int
               -> TotalMap (Alphabet fsm) Int
               -> ConstructionM fsm ()
setTransitions src map =
    modify $ \s -> s { csTransitions = IM.insert src map (csTransitions s) }

addBackEdge :: Int
            -> Int
            -> ConstructionM fsm ()
addBackEdge tgt src = do
  srcs <- (join . maybeToList . IM.lookup tgt) <$> gets csBackEdges
  modify $ \cs -> cs { csBackEdges = IM.insert tgt (src:srcs) (csBackEdges cs) }

newState :: FiniteStateMachine fsm =>
            fsm
         -> State fsm
         -> ConstructionM fsm Int
newState r st = do
  q <- gets csNextState
  modify $ \cs -> cs { csNextState = csNextState cs + 1
                     , csVisited   = M.insert st q (csVisited cs)
                     , csAccepting = case isAcceptingState r st of
                                       Nothing -> csAccepting cs
                                       Just x  -> IM.insert q x (csAccepting cs)
                     }
  return q

-- | Main DFA generation function.
explore :: FiniteStateMachine fsm =>
           fsm
        -> State fsm
        -> ConstructionM fsm Int
explore r q = do
  visited <- haveVisited q
  case visited of
    Nothing -> do
      s <- newState r q
      let doClass c = do
            s' <- explore r (advance r c q)
            addBackEdge s' s
            return s'
      t <- makeTotalMapA doClass (classes r q)
      setTransitions s t
      return s
    Just s -> return s

-- Determines the set of reachable states from a given state using the
-- provided edges. This function is used to discover which states are
-- unrecoverable error states in generated automata.
findReachable :: IM.IntMap [Int] -> Int -> IS.IntSet
findReachable edges s = execState (go s) IS.empty
    where
      go s = do
        visited <- gets (IS.member s)
        unless visited $ do
          modify (IS.insert s)
          mapM_ go (join $ maybeToList $ IM.lookup s edges)

-- Uses 'findReachable' to determine the set of states reachable from
-- a list of states. This function is used to discover which states
-- are unrecoverable error states in generated automata.
findAllReachable :: IM.IntMap [Int] -> [Int] -> IS.IntSet
findAllReachable edges = foldMap (findReachable edges)

-- | Construct a 'DFA' from a 'FiniteStateMachine'.
makeDFA :: FiniteStateMachine fsm =>
           fsm
        -> DFA (Alphabet fsm) (Result fsm)
makeDFA r = DFA transitions error final
    where
      init = CS M.empty 0 IM.empty IM.empty IM.empty

      CS _ next trans backEdges final =
          execState (explore r (initState r)) init

      finalReaching =
          findAllReachable backEdges (IM.keys final)

      error =
          IS.fromList [ i | i <- [0..next-1], not (IS.member i finalReaching) ]

      transitions =
          array (0,next-1) (IM.assocs trans)

{------------------------------------------------------------------------------}
-- | Representation of the result of stepping a 'DFA'.
data TransitionResult a
    = Accept a !Int
    | Error
    | Change !Int
    deriving (Eq, Ord, Show)

-- | Step a 'DFA' in a given state on a given input. If an error state
-- is reached, then this fact is returned instead of the actual state
-- name.
transition :: Ord a =>
              DFA a b -- ^ a deterministic finite automaton
           -> Int -- ^ state number
           -> a -- ^ input token
           -> TransitionResult b -- ^ the result of this transition
transition dfa state c = result
    where
      DFA transitions errorStates acceptingStates = dfa
      
      newState = (transitions ! state) $@ c

      result = if IS.member newState errorStates then Error
               else case IM.lookup newState acceptingStates of
                      Nothing -> Change newState
                      Just a  -> Accept a newState
