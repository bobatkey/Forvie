module Data.DFA.Elisp
    ( makeTransitionFunction 
    , makeTransitionFunctionCharTables )
    where

import           Data.Array (assocs, range, bounds)
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import           Data.RangeSet (ranges)
import qualified Data.RangeSet as RS
import           Data.Char (ord)
import           Data.Maybe (mapMaybe)
import           Data.SExpr
import           Data.DFA

-- turn the DFA into a collection of 'char-table's, one for each
-- state, plus a vector containing all these char-tables
makeTransitionFunctionCharTables :: ShowSExpr a => String -> DFA Char a -> [SExpr]
makeTransitionFunctionCharTables prefix dfa =
    [ SExpr [ Atom "defconst"
            , Atom (prefix ++ "-transition-vector")
            , SExpr ([ Atom "let", SExpr letBindings ]
                     ++
                     concatMap charTableEntries (assocs transitions)
                     ++ 
                     [ SExpr (Atom "vector":map (Atom . charTableName) (range $ bounds transitions))
                     ])
            ]
    , SExpr [ Atom "defun"
            , Atom (prefix ++ "-transition-function")
            , SExpr [ Atom "state", Atom "char" ]
            , SExpr [ Atom "aref"
                    , SExpr [ Atom "aref"
                            , Atom (prefix ++ "-transition-vector")
                            , Atom "state"
                            ]
                    , Atom "char"
                    ]
            ]
    ]
    where
      DFA transitions errorStates acceptingStates = dfa

      charTableName q = "s-" ++ show q

      letBindings = map makeLetBinding $ range $ bounds transitions
          where makeLetBinding q = SExpr [ Atom (charTableName q)
                                         , SExpr [ Atom "make-char-table"
                                                 , Atom "'lexer-table"
                                                 , SExpr [ Atom "list", Atom "'error" ]
                                                 ]
                                         ]
                                                

      charTableEntries (q,trans) =
          map (makeCharTableEntry (charTableName q)) (flattenCSets $ RS.assocs trans)

      makeCharTableEntry name (low,high,res)
          | low == high = SExpr [ Atom "set-char-table-range"
                                , Atom name
                                , IntConst (ord high)
                                , res ]
          | otherwise   = SExpr [ Atom "set-char-table-range"
                                , Atom name
                                , SExpr [ Atom "cons", IntConst (ord low), IntConst (ord high) ]
                                , res
                                ]

      mkResult q =
        if q `IS.member` errorStates then Nothing -- SExpr [ Atom "list", Atom "'error" ]
        else case IM.lookup q acceptingStates of
               Nothing -> Just $ SExpr [ Atom "list", Atom "'change", IntConst q ]
               Just t  -> Just $ SExpr [ Atom "list", Atom "'accept", IntConst q, showSExpr t ]
                           
      flattenCSets = concat . mapMaybe flattenCSet 
          where
            flattenCSet (cset, q) = do res <- mkResult q
                                       return $ map (\(low,high) -> (low, high, res)) (ranges cset)


makeTransitionFunction :: ShowSExpr a => String -> DFA Char a -> [SExpr]
makeTransitionFunction name dfa =
    [ SExpr [ Atom "defun"
            , Atom (name ++ "-transition-function")
            , SExpr [ Atom "state"
                    , Atom "char"
                    ]
            , cond clauses
            ]
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
