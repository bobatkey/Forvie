{-# LANGUAGE TypeFamilies, RankNTypes #-}

-- |
-- Module          :  Data.NFA
-- Copyright       :  (C) Robert Atkey 2013
-- License         :  BSD3
--
-- Maintainer      :  bob.atkey@gmail.com
-- Stability       :  experimental
-- Portability     :  unknown
--
-- Nondeterministic finite automata.

module Data.NFA
    ( -- * Nondeterministic Finite Automata
      NFA ()

      -- * Construction of NFAs via a monadic interface
      -- $constructionexample
    , NFAConstruction
    , St ()
    , toNFA
    , state
    , acceptingState )
    where

import           Prelude hiding (or)
import           Data.Monoid (Monoid (..))
import           Data.Foldable (fold, foldMap)
import           Data.Maybe (fromMaybe)
import           Control.FiniteStateMachine (FiniteStateMachine (..))
import           Control.Monad.Fix (MonadFix (..))
import           Data.BooleanAlgebra (or)
import           Data.Functor ((<$>))
import qualified Data.RangeSet as RS
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

-- | Representation of Nondeterministic Finite Automata (NFAs). An NFA
-- of type @NFA alphabet result@ operates on input symbols of type
-- @alphabet@, and accepting states are labelled with values of type
-- @result@.
data NFA alphabet result = NFA
    { nfaInputTransitions   :: IM.IntMap (M.Map alphabet IS.IntSet)
    , nfaEpsilonTransitions :: IM.IntMap IS.IntSet
    , nfaAcceptingStates    :: IM.IntMap result
    } deriving (Show, Eq, Ord)

-- | Modification of result values.
instance Functor (NFA alphabet) where
    fmap f nfa =
        nfa { nfaAcceptingStates = fmap f (nfaAcceptingStates nfa) }

--------------------------------------------------------------------------------
-- Construction of NFAs

-- $constructionexample
-- FIXME: do an example of using the NFA construction interface


-- | Monad used to construct NFAs incrementally. FIXME: do an example.
newtype NFAConstruction s alphabet result a = NFAConstruction
    { unNFAConstruction :: Int
                        -> NFA alphabet result
                        -> (Int, NFA alphabet result, a) }

-- | Abstract type of NFA states for an under construction NFA. The
-- type tag @s@ links a value of type @St s@ to a particular
-- construction process, ensuring that states from different NFAs
-- cannot be confused.
newtype St s = St { unSt :: Int }

-- | Handles generation of fresh state names and gathering of
-- transition maps.
instance Monad (NFAConstruction s alphabet result) where
    return a =
        NFAConstruction $ \nextState nfa ->
            (nextState, nfa, a)

    NFAConstruction c >>= f =
        NFAConstruction $ \nextState0 nfa0 ->
            let (nextState1, nfa1, a) = c nextState0 nfa0
            in unNFAConstruction (f a) nextState1 nfa1

-- | Allow construction of NFAs with loops.
instance MonadFix (NFAConstruction s alphabet result) where
    mfix f =
        NFAConstruction $ \nextState nfa ->
            let (nextState', nfa', a) = unNFAConstruction (f a) nextState nfa
            in (nextState', nfa', a)

-- | Add a state to an under construction NFA.
state :: Ord alphabet =>
         [ (alphabet, St s) ] -- ^ State transitions on input tokens from the new state
      -> [ St s ] -- ^ Epsilon transitions from the new state
      -> NFAConstruction s alphabet result (St s)
state inputTrans epsTrans =
    NFAConstruction $ \nextState nfa ->
        let thisState = nextState

            newEpsilonTransitions =
                IM.insert thisState
                  (IS.fromList $ fmap unSt epsTrans)
                  (nfaEpsilonTransitions nfa)

            inputTransitionMap =
                foldr (\(inpTok,st) ->
                           M.insertWith IS.union inpTok (IS.singleton (unSt st)))
                    M.empty
                    inputTrans

            newInputTransitions =
                IM.insert thisState
                  inputTransitionMap
                  (nfaInputTransitions nfa) in
        ( nextState+1
        , nfa { nfaInputTransitions = newInputTransitions
              , nfaEpsilonTransitions = newEpsilonTransitions
              }
        , St thisState )

-- | Add an accepting state to an under construction NFA.
acceptingState :: Ord alphabet =>
                  [ (alphabet, St s) ] -- ^ State transitions on input tokens from the new state
               -> [ St s ] -- ^ Epsilon transitions from the new state
               -> result -- ^ Result value associated with the current state
               -> NFAConstruction s alphabet result (St s)
acceptingState inputTrans epsTrans result = do
  s <- state inputTrans epsTrans
  setAccepting s result
  return s

setAccepting :: St s -> result -> NFAConstruction s alphabet result ()
setAccepting (St q) result =
    NFAConstruction $ \nextState nfa ->
        ( nextState
        , nfa { nfaAcceptingStates =
                    IM.insert q result (nfaAcceptingStates nfa) }
        , () )

-- | Construct a concrete NFA from an 'NFAConstruction'.
toNFA :: (forall s. NFAConstruction s alphabet result ())
      -> NFA alphabet result
toNFA c = nfa
    where
      emptyNFA = NFA IM.empty IM.empty IM.empty

      (_, nfa, _) = unNFAConstruction c 0 emptyNFA

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

-- | Treat an @NFA alphabet result@ as a finite state machine using
-- the subset construction. The @Monoid result@ constraint should be
-- read to indicate a *commutative* monoid.
instance (Enum alphabet, Ord alphabet, Bounded alphabet, Monoid result) =>
         FiniteStateMachine (NFA alphabet result)
    where
    type State (NFA alphabet result)    = IS.IntSet
    type Alphabet (NFA alphabet result) = alphabet
    type Result (NFA alphabet result)   = result

    initState nfa = closeUnderEpsilon nfa (IS.singleton 0)

    advance nfa a stateSet = closeUnderEpsilon nfa newStateSet
        where
          newStateSet =
              IS.unions $ map (inputTransition nfa a) $ IS.elems stateSet

    isAcceptingState nfa stateSet =
        mconcat $
        map (\q -> IM.lookup q (nfaAcceptingStates nfa)) $
        IS.elems stateSet

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
