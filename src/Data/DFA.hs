{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}

-- |
-- Module           :  Data.DFA
-- Copyright        :  Robert Atkey 2012
-- License          :  BSD3
--
-- Maintainer       :  bob.atkey@gmail.com
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
    , TransitionResult (..)
    , transition

      -- * Construction
    , FiniteStateAcceptor (..)
    , makeDFA
    , runFSA
    )
    where

import           Data.Foldable (foldMap)
import           Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Array (Array, array, (!))
import           Data.RangeSet
import qualified Control.Monad.State as S
import           Control.Monad.State (modify, gets, execState, join, unless)
import           Control.Applicative ((<$>))

{------------------------------------------------------------------------------}
class ( Ord (State fsa)
      , Enum (Alphabet fsa)
      , Ord (Alphabet fsa)
      , Bounded (Alphabet fsa)) => FiniteStateAcceptor fsa where
    type State fsa    :: *
    type Alphabet fsa :: *
    type Result fsa   :: *

    initState        :: fsa -> State fsa
    advance          :: fsa -> Alphabet fsa -> State fsa -> State fsa
    isAcceptingState :: fsa -> State fsa -> Maybe (Result fsa)
    classes          :: fsa -> State fsa -> Partition (Alphabet fsa)

-- really, vectors, but I can't be bothered
instance FiniteStateAcceptor fsa => FiniteStateAcceptor [fsa] where
    type State [fsa]    = [State fsa]
    type Alphabet [fsa] = Alphabet fsa
    type Result [fsa]   = Result fsa

    initState r = map initState r
    advance r c s = map (\(r,s) -> advance r c s) $ zip r s
    isAcceptingState r =
        listToMaybe . mapMaybe (\(r,s) -> isAcceptingState r s) . zip r
    classes r = foldMap (uncurry classes) . zip r

runFSA :: FiniteStateAcceptor fsa =>
                          fsa
                       -> [Alphabet fsa]
                       -> Maybe (Result fsa)
runFSA fsa input
    = run (initState fsa) input
    where
      run q []     = isAcceptingState fsa q
      run q (x:xs) = run (advance fsa x q) xs

{------------------------------------------------------------------------------}
-- DFA construction
data ConstructorState re
    = CS { csVisited     :: !(M.Map (State re) Int)
         , csNextState   :: !Int
         , csTransitions :: !(IM.IntMap (TotalMap (Alphabet re) Int))
         , csBackEdges   :: !(IM.IntMap [Int])
         , csAccepting   :: !(IM.IntMap (Result re))
         }

type ConstructorM re a = S.State (ConstructorState re) a

haveVisited :: Ord (State fsa) => State fsa -> ConstructorM fsa (Maybe Int)
haveVisited q = gets (M.lookup q . csVisited)

setTransitions :: FiniteStateAcceptor fsa =>
                  Int
               -> TotalMap (Alphabet fsa) Int
               -> ConstructorM fsa ()
setTransitions src map =
    modify $ \s -> s { csTransitions = IM.insert src map (csTransitions s) }

addBackEdge :: Int
            -> Int
            -> ConstructorM fsa ()
addBackEdge tgt src = do
  srcs <- (join . maybeToList . IM.lookup tgt) <$> gets csBackEdges
  modify $ \cs -> cs { csBackEdges = IM.insert tgt (src:srcs) (csBackEdges cs) }

newState :: FiniteStateAcceptor fsa =>
            fsa
         -> State fsa
         -> ConstructorM fsa Int
newState r st = do
  q <- gets csNextState
  modify $ \cs -> cs { csNextState = csNextState cs + 1
                     , csVisited   = M.insert st q (csVisited cs)
                     , csAccepting = case isAcceptingState r st of
                                       Nothing -> csAccepting cs
                                       Just x  -> IM.insert q x (csAccepting cs)
                     }
  return q

explore :: FiniteStateAcceptor fsa =>
           fsa
        -> State fsa
        -> ConstructorM fsa Int
explore r q = do
  visited <- haveVisited q
  case visited of
    Nothing -> do
      s <- newState r q
      t <- makeTotalMapA (classes r q) $ \c -> do
                 s' <- explore r (advance r c q)
                 addBackEdge s' s
                 return s'
      setTransitions s t
      return s
    Just s -> return s

findReachable :: IM.IntMap [Int] -> Int -> IS.IntSet
findReachable edges s = execState (go s) IS.empty
    where
      go s = do
        visited <- gets (IS.member s)
        unless visited $ do
          modify (IS.insert s)
          mapM_ go (join $ maybeToList $ IM.lookup s edges)

findAllReachable :: IM.IntMap [Int] -> [Int] -> IS.IntSet
findAllReachable edges = foldMap (findReachable edges)

-- | The 'DFA' type has two parameters, the type of input tokens 'a'
-- and the type of output annotations for final states 'b'.
--
-- The states of a DFA are represented as 'Int' values in the range
-- '[0..n]', where 'n' is the number of states of the
-- automaton.
data DFA a b =
    DFA { -- | Transition functions of the DFA, indexed by state number.
          transitions :: !(Array Int (TotalMap a Int))
          -- | The set of error states. Transitions from states in this set will always lead back to this set, and never to an accepting state.
        , errorStates :: !IS.IntSet
          -- | The set of accepting states, with final values.
        , finalStates :: !(IM.IntMap b)
        }
    deriving (Eq, Ord, Show)

instance (Enum a, Bounded a, Ord a) => FiniteStateAcceptor (DFA a b) where
    type State (DFA a b)    = Int
    type Alphabet (DFA a b) = a
    type Result (DFA a b)   = b
    initState dfa          = 0
    advance dfa a q        = (transitions dfa ! q) $@ a
    isAcceptingState dfa q = IM.lookup q (finalStates dfa)
    classes dfa q          = domain (transitions dfa ! q)

instance Functor (DFA a) where
    fmap f dfa =
        dfa { finalStates = fmap f (finalStates dfa) }

makeDFA :: FiniteStateAcceptor fsa => fsa -> DFA (Alphabet fsa) (Result fsa)
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
      
      newState = (transitions ! state) $@ c

      result = if IS.member newState errorStates then Error
               else case IM.lookup newState acceptingStates of
                      Nothing -> Change newState
                      Just a  -> Accepting a newState
