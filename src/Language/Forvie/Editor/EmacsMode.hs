module Language.Forvie.Editor.EmacsMode
    ( generateElisp )
    where

import           Data.SExpr
import           Data.DFA.Elisp (makeTransitionFunction)
import           Language.Forvie.Lexing.Spec

instance ShowSExpr Classification where
    showSExpr Comment     = Atom "'comment"
    showSExpr Keyword     = Atom "'keyword"
    showSExpr Identifier  = Atom "'identifier"
    showSExpr Punctuation = Atom "'punctuation"
    showSExpr Whitespace  = Atom "'whitespace"
    showSExpr Constant    = Atom "'constant"
    showSExpr Operator    = Atom "'operator"
    showSExpr Constructor = Atom "'constructor"

generateElisp :: SyntaxHighlight tok =>
                 CompiledLexSpec tok ->
                 SExpr
generateElisp =
    makeTransitionFunction . fmap lexicalClass . lexSpecDFA
