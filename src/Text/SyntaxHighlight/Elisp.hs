module Text.SyntaxHighlight.Elisp
    ( generateElisp )
    where

import           Data.Array (assocs)
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM
import           Data.RangeSet (ranges)
import           Data.Char (ord)
import           Text.SExpr
import           Data.DFA.Elisp (makeTransitionFunction)
import           Text.LexicalSpecification

instance ShowSExpr Classification where
    showSExpr Comment     = Atom "'comment"
    showSExpr Keyword     = Atom "'keyword"
    showSExpr Identifier  = Atom "'identifier"
    showSExpr Punctuation = Atom "'punctuation"
    showSExpr Whitespace  = Atom "'whitespace"
    showSExpr Constant    = Atom "'constant"
    showSExpr Operator    = Atom "'operator"

generateElisp :: SyntaxHighlight tok =>
                 CompiledLexSpec tok ->
                 SExpr
generateElisp =
    makeTransitionFunction . fmap lexicalClass . lexSpecDFA
