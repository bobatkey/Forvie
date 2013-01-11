{-# LANGUAGE TypeFamilies #-}

module Data.NFA
    ( NFA (..)
    , nfa1
    , nfa2 )
    where

import           Prelude hiding (or)
import           Data.Monoid (Monoid (..))
import           Data.Foldable (fold, foldMap)
import           Data.Maybe (fromMaybe)
import           Control.FiniteStateMachine (FiniteStateMachine (..))
import           Data.BooleanAlgebra (or)
import           Data.Functor ((<$>))
import qualified Data.RangeSet as RS
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

--------------------------------------------------------------------------------
data NFA a b =
    NFA { nfaInputTransitions   :: IM.IntMap (M.Map a IS.IntSet)
        , nfaEpsilonTransitions :: IM.IntMap IS.IntSet
        , nfaAcceptingStates    :: IM.IntMap b
        }
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
epsilonTransition :: NFA a b -> Int -> IS.IntSet
epsilonTransition nfa q =
    fromMaybe IS.empty $ IM.lookup q $ nfaEpsilonTransitions nfa

closeUnderEpsilon :: NFA a b -> IS.IntSet -> IS.IntSet
closeUnderEpsilon nfa states = loop states states
    where
      loop done newStates
          | IS.null newStates = done
          | otherwise = loop (IS.union s done) (IS.difference s done)
          where s = IS.unions $ map (epsilonTransition nfa) $ IS.elems newStates

inputTransition :: Ord a => NFA a b -> a -> Int -> IS.IntSet
inputTransition nfa a q =
    fromMaybe IS.empty $ do
      tokenMap <- IM.lookup q (nfaInputTransitions nfa)
      M.lookup a tokenMap

possibleInputs :: NFA a b -> Int -> [a]
possibleInputs nfa q =
    fromMaybe [] (M.keys <$> IM.lookup q (nfaInputTransitions nfa))

-- | Treat an 'NFA a b' as a finite state machine using the subset
-- construction. The 'Monoid b' constraint should also indicate a
-- *commutative* monoid.
instance (Enum a, Ord a, Bounded a, Monoid b) =>
         FiniteStateMachine (NFA a b)
    where
    type State (NFA a b)    = IS.IntSet
    type Alphabet (NFA a b) = a
    type Result (NFA a b)   = b

    initState nfa = closeUnderEpsilon nfa (IS.singleton 0)

    advance nfa a stateSet = closeUnderEpsilon nfa newStateSet
        where
          newStateSet =
              IS.unions $ map (inputTransition nfa a) $ IS.elems stateSet

    isAcceptingState nfa stateSet =
        mconcat $ map (\q -> IM.lookup q (nfaAcceptingStates nfa)) $ IS.elems stateSet

    classes nfa state = partition
        where
          transitionGroups =
              groups $ fold [ singletonGrouping (advance nfa a state) a |
                              q <- IS.elems state
                            , a <- possibleInputs nfa q
                            ]

          partition =
              foldMap (RS.fromSet . or . S.map RS.singleton) transitionGroups

--------------------------------------------------------------------------------
newtype Grouping a b =
    Grouping { unGrouping :: M.Map a (S.Set b) }

groups :: Ord a => Grouping a b -> [S.Set b]
groups =
    M.elems . unGrouping

singletonGrouping :: (Ord a, Ord b) => a -> b -> Grouping a b
singletonGrouping a b =
    Grouping (M.singleton a (S.singleton b))

instance (Ord a, Ord b) => Monoid (Grouping a b) where
    mempty = Grouping M.empty
    mappend (Grouping map1) (Grouping map2) =
        Grouping (M.unionWith S.union map1 map2)

--------------------------------------------------------------------------------
-- a test NFA
nfa1 :: NFA Char ()
nfa1 = NFA { nfaInputTransitions   = IM.fromList [ (0, M.fromList [ ('a', IS.fromList [ 0, 1 ])
                                                                  , ('b', IS.fromList [ 1 ])
                                                                  ])
                                                 , (1, M.fromList [ ('a', IS.fromList [ 0 ])
                                                                  ])
                                                 ]
           , nfaEpsilonTransitions = IM.fromList [ ]
           , nfaAcceptingStates    = IM.fromList [ (0, ()) ]
           }

nfa2 :: NFA Char String
nfa2 = NFA
       { nfaInputTransitions   = IM.fromList [ ]
       , nfaEpsilonTransitions = IM.fromList [ (0, IS.fromList [1])
                                             , (1, IS.fromList [2]) ]
       , nfaAcceptingStates    = IM.fromList [ (0, "a"), (1, "b") ]
       }
