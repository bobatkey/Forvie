{-# LANGUAGE PackageImports, TypeFamilies, FlexibleInstances #-}

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
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Array (Array, array, (!))
import           Data.RangeSet
import "mtl"     Control.Monad.State

{------------------------------------------------------------------------------}
class Ord r => FiniteStateAcceptor r where
    type Result r :: *
    diff           :: Char -> r -> r
    matchesNothing :: r -> Bool
    matchesEmpty   :: r -> Maybe (Result r)
    classes        :: r -> Partition Char

{------------------------------------------------------------------------------}
-- DFA construction
data ConstructorState re
    = CS { csStates      :: M.Map re Int
         , csNextState   :: Int
         , csTransitions :: IM.IntMap (TotalMap Char Int)
         , csErrorStates :: IS.IntSet
         , csFinalStates :: IM.IntMap (Result re)
         }

type ConstructorM re a = State (ConstructorState re) a

haveVisited :: Ord re => re -> ConstructorM re (Maybe Int)
haveVisited r = get >>= return . M.lookup r . csStates

setTransitions :: Int -> TotalMap Char Int -> ConstructorM re ()
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

data DFA a =
    DFA { numStates   :: Int
        , transitions :: Array Int (TotalMap Char Int)
        , errorStates :: IS.IntSet
        , finalStates :: IM.IntMap a
        }
    deriving Show

instance Functor DFA where
    fmap f dfa =
        dfa { finalStates = fmap f (finalStates dfa) }

makeDFA :: FiniteStateAcceptor re => re -> DFA (Result re)
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

transition :: DFA a -> Int -> Char -> TransitionResult a
transition dfa state c = result
    where
      DFA _ transitions errorStates acceptingStates = dfa
      
      newState = lookup (transitions ! state) c
      
      result = if IS.member newState errorStates then Error
               else case IM.lookup newState acceptingStates of
                      Nothing -> Change newState
                      Just a  -> Accepting a newState

runDFA :: DFA a -> String -> Maybe a
runDFA dfa = aux 0
    where
      DFA _ transitions _ final = dfa

      aux s []     = IM.lookup s final
      aux s (c:cs) = aux (lookup (transitions ! s) c) cs
