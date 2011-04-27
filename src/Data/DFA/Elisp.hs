module Data.DFA.Elisp
    ( makeTransitionFunction )
    where

import           Data.Array (assocs)
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import           Data.RangeSet (ranges)
import qualified Data.RangeSet as RS
import           Data.Char (ord)
import           Data.Maybe (mapMaybe)
import           Data.SExpr
import           Data.DFA

makeTransitionFunction :: ShowSExpr a => DFA Char a -> SExpr
makeTransitionFunction dfa =
    SExpr [ Atom "defun"
          , Atom "transition-function"
          , SExpr [ Atom "state"
                  , Atom "char"
                  ]
          , cond clauses
          ]
    where
      DFA transitions errorStates acceptingStates = dfa
      
      clauses = map doState $ assocs transitions
      
      doState (q, trans) = ( SExpr [ Atom "=", Atom "state", IntConst q ]
                           , cond $ (map doTrans $ flattenCSets $ RS.assocs trans)
                                    ++
                                    [( Atom "t", SExpr [ Atom "list", Atom "'error" ])]
                           )
                           
      doTrans (low, high, res)
          | low == high = ( SExpr [ Atom "=", Atom "char", IntConst (ord high) ], res)
          | otherwise   = ( SExpr [ Atom "and"
                                  , SExpr [ Atom "<=", IntConst (ord low), Atom "char" ]
                                  , SExpr [ Atom "<=", Atom "char", IntConst (ord high) ]     
                                  ]
                          , res)
      
      mkResult q =
        if q `IS.member` errorStates then Nothing -- SExpr [ Atom "list", Atom "'error" ]
        else case IM.lookup q acceptingStates of
               Nothing -> Just $ SExpr [ Atom "list", Atom "'change", IntConst q ]
               Just t  -> Just $ SExpr [ Atom "list", Atom "'accept", IntConst q, showSExpr t ]
                           
      flattenCSets = concat . mapMaybe flattenCSet 
          where
            flattenCSet (cset, q) = do res <- mkResult q
                                       return $ map (\(low,high) -> (low, high, res)) (ranges cset)
