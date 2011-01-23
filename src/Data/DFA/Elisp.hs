module Data.DFA.Elisp
    ( makeTransitionFunction )
    where

import           Data.Array (assocs)
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import           Data.RangeSet (ranges)
import           Data.Char (ord)
import           Text.SExpr
import           Data.DFA

{-
toAtom Comment     = "'comment"
toAtom Keyword     = "'keyword"
toAtom Identifier  = "'identifier"
toAtom Punctuation = "'punctuation"
toAtom Whitespace  = "'whitespace"
toAtom Constant    = "'constant"
toAtom Operator    = "'operator"
-}

makeTransitionFunction :: ShowSExpr a => DFA a -> SExpr
makeTransitionFunction dfa =
    SExpr [ Atom "defun"
          , Atom "transition-function"
          , SExpr [ Atom "state"
                  , Atom "char"
                  ]
          , cond clauses
          ]
    where
      DFA _ transitions errorStates acceptingStates = dfa
      
      clauses = map doState $ assocs transitions
      
      doState (q, trans) = ( [ Atom "=", Atom "state", IntConst q ]
                           , cond $ map doTrans $ flattenCSets trans
                           )
                           
      doTrans (low, high, res) =
        ( [ Atom "and"
          , SExpr [ Atom "<=", IntConst (ord low), Atom "char" ]
          , SExpr [ Atom "<=", Atom "char", IntConst (ord high) ]     
          ]
        , res
        )
      
      mkResult q =
        if q `IS.member` errorStates then SExpr [ Atom "list", Atom "'error" ]
        else case IM.lookup q acceptingStates of
               Nothing -> SExpr [ Atom "list", Atom "'change", IntConst q ]
               Just t  -> SExpr [ Atom "list", Atom "'accept", IntConst q, showSExpr t ]
                           
      flattenCSets = concat . map flattenCSet 
          where
            flattenCSet (cset, q) = let res = mkResult q in map (\(low,high) -> (low, high, res)) (ranges cset)
