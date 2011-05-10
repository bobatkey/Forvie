{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, FlexibleInstances, GADTs #-}

-- | Implementation of the Foveran parsing process, from bytes on disk
-- up to abstract syntax trees in "Foveran.Syntax.Display" form.

module Parsing
    ( readFoveranFile
    , lexer
    , ppInputError
    , prettyKnot
    , readFoveranFile2
    , lexFoveranFile
    )
    where

import Data.Knot

import Data.ByteString (ByteString)
import Data.Type.Show
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.PrettyPrint
import Text.Position
import Control.Applicative
import Control.StreamProcessor
import Control.StreamProcessor.ByteString
import Control.StreamProcessor.UTF8
import Control.StreamProcessor.IO

import Language.Forvie.Lexing.Spec
import Language.Forvie.Lexing.Generator
import Language.Forvie.Parsing.EarleyParser

import Display (Declaration, AST, File, prettyKnot)

import Token
import LexicalSpec
import Grammar

--------------------------------------------------------------------------------
ppPos p =
  text "line" <+> int (posLineNum p) <> comma <+> text "col" <+> int (posColumnNum p)

ppSpan (Span l r) =
  text "from" <+> ppPos l <+> text "to" <+> ppPos r

--------------------------------------------------------------------------------
data InputError
    = PE_UTF8DecodeError String
    | PE_LexingError     (Maybe (Char, Position))
    | PE_ParsingError    (Maybe (Lexeme Token)) [Maybe Token]

instance UTF8DecodeError InputError where
    utf8DecodeError = PE_UTF8DecodeError

instance LexingError InputError where
    lexingErrAtEOS       = PE_LexingError Nothing
    lexingErrOnInput c p = PE_LexingError (Just (c,p))

{-    
instance ParsingError Token InputError where
    parseError = PE_ParsingError
-}

--------------------------------------------------------------------------------
ppToken Nothing  = "End of file"
ppToken (Just t) = text $ show t

ppExpecting [] =
    "Expecting nothing"
ppExpecting [x] =
    "Expecting" <+> ppToken x
ppExpecting l =
    "Expecting one of" $$ nest 4 (hsep (map ppToken l))

ppInputError :: InputError -> Doc
ppInputError (PE_UTF8DecodeError s) =
    "UTF-8 Decoding error" <> colon <+> text s
ppInputError (PE_LexingError Nothing) =
    "Lexing error at the end of the file, incomplete token"
ppInputError (PE_LexingError (Just (c, p))) =
    "Lexing error at" <+> ppPos p <+> "on input" <+> char c
ppInputError (PE_ParsingError Nothing expecting) =
    "Parsing error at end of the file" $$ ppExpecting expecting
ppInputError (PE_ParsingError (Just (Lexeme _ p s)) expecting) =
    "Parse error" <+> ppSpan p <+> "on input" <+> text (T.unpack s)
    $$ ppExpecting expecting

--------------------------------------------------------------------------------

lexer' :: SP InputError Char (Lexeme (Action Token))
lexer' = $(lexerSPStatic (compileLexicalSpecification lexicalSpec))

lexer :: SP InputError ByteString (Lexeme (Action Token))
lexer =
    toWord8 >>>
    decodeUTF8 >>>
    lexer'

--------------------------------------------------------------------------------
-- FIXME: move this elsewhere
instance {-Show4 f =>-} ParseResultsMonad f (Knot f) (SR InputError c) where
    newResult i j x      = Yield (Knot x)
    addResult (Knot x) y = ReadError $ PE_UTF8DecodeError "Ambiguity detected: " -- ++ show (Knot x) ++ " vs " ++ show (Knot y)

instance ParseSourceMonad tok (SR e (Lexeme tok)) where
    getInput = Read Yield

--------------------------------------------------------------------------------
parser :: SR InputError ByteString (Knot AST File)
parser =
    lexer >>>
    exceptIgnorable >>|
    (do l <- parse grammar Decls ()
        case l of
          [a] -> return (Knot a)
          _   -> ReadError $ PE_UTF8DecodeError "parse failure")

parser' :: SR InputError Char (Knot AST File)
parser' =
    lexer' >>>
    exceptIgnorable >>|
    (do l <- parse grammar Decls ()
        case l of
          [a] -> return (Knot a)
          _   -> ReadError $ PE_UTF8DecodeError "parse failure")

--------------------------------------------------------------------------------
readFoveranFile :: FilePath -> IO (Either InputError (Knot AST File))
readFoveranFile filename = do
  onFile filename 8192 parser

--------------------------------------------------------------------------------
data AmbiguityCheckMonad f a where
    Success           :: a -> AmbiguityCheckMonad f a
    AmbiguityDetected :: Knot f x -> Knot f x -> AmbiguityCheckMonad f a
    ParseFailure      :: AmbiguityCheckMonad f a

instance (Show4 f, Show a) => Show (AmbiguityCheckMonad f a) where
    show (Success a)             = "Success " ++ show a
    show (AmbiguityDetected x y) = "AmbiguityDetected " ++ show2 x ++ " " ++ show2 y
    show (ParseFailure)          = "ParseFailure"

instance Monad (AmbiguityCheckMonad f) where
    return = Success
    Success a             >>= f = f a
    AmbiguityDetected x y >>= _ = AmbiguityDetected x y

instance Functor (AmbiguityCheckMonad f) where
    fmap f (Success x)             = Success (f x)
    fmap f (AmbiguityDetected x y) = AmbiguityDetected x y

instance ParseResultsMonad f (Knot f) (AmbiguityCheckMonad f) where
    newResult i j x = Success (Knot x)
    addResult x y   = AmbiguityDetected x (Knot y)

rejectAmbiguity :: AmbiguityCheckMonad f [f (Knot f) t] -> AmbiguityCheckMonad f (Knot f t)
rejectAmbiguity (Success [])            = ParseFailure
rejectAmbiguity (Success [x])           = Success (Knot x)
rejectAmbiguity (Success (x:y:_))       = AmbiguityDetected (Knot x) (Knot y)
rejectAmbiguity (AmbiguityDetected x y) = AmbiguityDetected x y
rejectAmbiguity ParseFailure            = ParseFailure

lexFoveranFile :: FilePath -> IO (Either InputError [Lexeme Token])
lexFoveranFile filename = do
  onText (lexer' >>> exceptIgnorable >>| gather) <$> TIO.readFile filename

readFoveranFile2 :: FilePath -> IO (Either InputError (Knot AST File))
readFoveranFile2 filename = do
  input <- onText (lexer' >>> exceptIgnorable >>| gather) <$> TIO.readFile filename
  case input of
    Left e -> return $ Left e
    Right tokens -> case rejectAmbiguity $ parseList grammar Decls () tokens of
                      ParseFailure -> return $ Left (PE_UTF8DecodeError "Parse failure")
                      AmbiguityDetected x y -> return $ Left (PE_UTF8DecodeError "Ambiguity detected")
                      Success x    -> return $ Right x