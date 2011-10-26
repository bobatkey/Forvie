{-# LANGUAGE TemplateHaskell #-}

module Data.DFA.TH
    (makeTransitionFunction)
    where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Array (assocs)
import           Data.DFA
import           Data.RangeSet hiding (assocs)
import qualified Data.RangeSet as RS
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

-- convert DFAs to functions that execute them

-- plan is to generate a massive pattern match on the state and the
-- input character. This will return the result as a
-- "TransitionResult"

makeTransitionFunction :: (Lift c, Lift a) => DFA c a -> ExpQ
makeTransitionFunction (DFA transitions errorStates acceptingStates)
    = lamE [ varP state, varP c ]
           (caseE (varE state) (map mkMatch $ assocs transitions))
      where
        mkMatch (q, trans) = match (litP (IntegerL $ fromIntegral q))
                                   (guardedB $ map mkCharMatch $ flattenCSets $ RS.assocs trans)
                                   []

        c = mkName "c"
        state = mkName "state"

        mkCharMatch (low, high, res) = normalGE [| $(varE c) >= low && $(varE c) <= high |] res

        mkResult q =
            if q `IS.member` errorStates then [| Error |]
            else case IM.lookup q acceptingStates of
                   Nothing -> [| Change q |]
                   Just a  -> [| Accepting a q |]

        flattenCSets = concat . map flattenCSet 
            where
              flattenCSet (cset, q) = let res = mkResult q in map (\(low,high) -> (low, high, res)) (ranges cset)
