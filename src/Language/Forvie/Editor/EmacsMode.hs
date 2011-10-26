{-# LANGUAGE OverloadedStrings #-}

module Language.Forvie.Editor.EmacsMode
    ( generateEmacsMode )
    where

import           Paths_forvie
import           System.IO
import           Data.SExpr
import           Text.PrettyPrint (render)
import           Data.DFA.Elisp (makeTransitionFunctionCharTables)
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
    showSExpr Type        = Atom "'type"

generateElisp :: SyntaxHighlight tok =>
                 String ->
                 CompiledLexSpec tok ->
                 [SExpr]
generateElisp name =
    makeTransitionFunctionCharTables name . fmap lexicalClass . lexSpecDFA

-- plan: generate an emacs mode by
--  (a) making the transition function code
--  (b) getting the mode-template.el and replacing everything with the modename and the fileregexp
--  (c) outputting the lot somewhere

-- | Generate the Emacs lisp code for a simple emacs major mode based
-- on the provided lexical specification.
--
-- The Emacs lisp code is sent to `stdout`.
--
-- The generated major mode has the following features:
-- 
-- * Accurate syntax highlighting based on the lexical specification
generateEmacsMode :: SyntaxHighlight tok =>
                     CompiledLexSpec tok -- ^ Lexical specification to be used for syntax highlighting
                  -> T.Text              -- ^ The name for this mode, used to prefix all the generated elisp functions
                  -> T.Text              -- ^ An emacs-style regular expression for filenames where this mode should be used
                  -> IO ()               -- ^ `IO` action that emits Elisp code to `stdout`
generateEmacsMode lexSpec modename fileregexp =
    do templateFilename <- getDataFileName "elisp/mode-template.el"
       mapM_ (putStrLn . render . pprint) (generateElisp (T.unpack modename) lexSpec)
       result <- onFiles templateFilename 8192 $ applyVariableSubstitution subst
       case result of
         Nothing    -> return ()
         Just error -> hPutStrLn stderr $ "Error: " ++ error
    where
      subst = [ ("modename",   modename)
              , ("fileregexp", fileregexp)
              ]