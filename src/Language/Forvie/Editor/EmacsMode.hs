{-# LANGUAGE OverloadedStrings #-}

module Language.Forvie.Editor.EmacsMode
    ( generateEmacsMode )
    where

import           Paths_forvie
import           System.IO
import           Data.SExpr
import           Text.PrettyPrint (render)
import           Data.DFA.Elisp (makeTransitionFunction)
import           Language.Forvie.Lexing.Spec
import           Language.Forvie.Util.Templater

import           Control.StreamProcessor.IO
import qualified Data.Text as T

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
                 String ->
                 CompiledLexSpec tok ->
                 SExpr
generateElisp name =
    makeTransitionFunction name . fmap lexicalClass . lexSpecDFA

-- plan: generate an emacs mode by
--  (a) making the transition function code
--  (b) getting the mode-template.el and replacing everything with the modename and the fileregexp
--  (c) outputting the lot somewhere

generateEmacsMode lexSpec modename fileregexp =
    do templateFilename <- getDataFileName "elisp/mode-template.el"
       putStrLn $ render $ pprint $ generateElisp (T.unpack modename ++ "-transition-function") lexSpec
       result <- onFiles templateFilename 8192 $ applyVariableSubstitution subst
       case result of
         Nothing    -> return ()
         Just error -> hPutStrLn stderr $ "Error: " ++ error
    where
      subst = [ ("modename",   modename)
              , ("fileregexp", fileregexp)
              ]