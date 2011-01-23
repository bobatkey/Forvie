{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances, FlexibleContexts #-}

-- implementation of the DFA construction algorithm from Owens et al

-- FIXME: make DFA type abstract and provide abstract transition
-- functions. Is there any way to tie the state type to the DFA? Rank
-- 2 polymorphism?

module Data.DFA
    ( FiniteStateAcceptor (..)
    , DFA (..)
    , makeDFA
    , runDFA
    , TransitionResult (..)
    , transition
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
    type Alphabet r :: *
    type Result r   :: *
    diff            :: Alphabet r -> r -> r
    matchesNothing  :: r -> Bool
    matchesEmpty    :: r -> Maybe (Result r)
    classes         :: r -> Partition (Alphabet r)

instance (FiniteStateAcceptor r, Ord a) => FiniteStateAcceptor [(r,a)] where
    type Alphabet [(r,a)] = Alphabet r
    type Result [(r,a)] = a
    diff c         = map (first $ diff c)
    matchesNothing = all (matchesNothing . fst)
    matchesEmpty   = fmap snd . find (isJust . matchesEmpty . fst)
    classes        = foldl andClasses (fromSet one) . map (classes . fst)

{------------------------------------------------------------------------------}
-- DFA construction
data ConstructorState re
    = CS { csStates      :: M.Map re Int
         , csNextState   :: Int
         , csTransitions :: IM.IntMap (TotalMap (Alphabet re) Int)
         , csErrorStates :: IS.IntSet
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
  CS states next trans error final <- get
  let states' = M.insert r next states
      next'   = next + 1
      error'  = if matchesNothing r then IS.insert next error else error
      final'  = case matchesEmpty r of
                  Nothing  -> final
                  Just res -> IM.insert next res final
  put (CS states' next' trans error' final')
  return next

explore :: FiniteStateAcceptor re => re -> ConstructorM re Int
explore q = do
  s <- newState q
  t <- makeTotalMapM (classes q)
       $ \c -> do let q' = diff c q
                  visited <- haveVisited q'
                  case visited of
                    Nothing -> explore q'
                    Just s  -> return s
  setTransitions s t
  return s

data DFA a b =
    DFA { numStates   :: Int
        , transitions :: Array Int (TotalMap a Int)
        , errorStates :: IS.IntSet
        , finalStates :: IM.IntMap b
        }
    deriving Show

instance Functor (DFA a) where
    fmap f dfa =
        dfa { finalStates = fmap f (finalStates dfa) }

makeDFA :: FiniteStateAcceptor re => re -> DFA (Alphabet re) (Result re)
makeDFA r = DFA next transArray error final
    where
      init = CS (M.fromList [ {-(r, 0)-} ]) 0 IM.empty IS.empty IM.empty

      CS states next trans error final = execState (explore r) init

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
      DFA _ transitions errorStates acceptingStates = dfa
      
      newState = lookup (transitions ! state) c
      
      result = if IS.member newState errorStates then Error
               else case IM.lookup newState acceptingStates of
                      Nothing -> Change newState
                      Just a  -> Accepting a newState

runDFA :: Ord a => DFA a b -> [a] -> Maybe b
runDFA dfa = aux 0
    where
      DFA _ transitions _ final = dfa

      aux s []     = IM.lookup s final
      aux s (c:cs) = aux (lookup (transitions ! s) c) cs
