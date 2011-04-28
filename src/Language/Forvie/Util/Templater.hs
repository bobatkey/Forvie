{-# LANGUAGE OverloadedStrings #-}

module Language.Forvie.Util.Templater where

import qualified Data.Map as M
import Data.ByteString (ByteString)
import Data.Text (Text, dropAround,pack)
import Data.Text.Encoding (encodeUtf8)
import Control.StreamProcessor
import Control.StreamProcessor.IO
import Control.StreamProcessor.UTF8
import Control.StreamProcessor.ByteString
import Control.StreamProcessor.Binary
import Language.Forvie.Lexing.Spec
import Language.Forvie.Lexing.Generator

data Token = Text | Variable deriving (Show, Eq, Ord)

lexicalSpec = compileLexicalSpecification
    [ ( "$" .>>. oneOrMore (tok (interval 'A' 'Z' .|. interval 'a' 'z' .|. interval '0' '9')) .>>. "$"
      , Variable)

    , ( oneOrMore (tok (complement (singleton '$')))
      , Text)

    , ( "$"
      , Text)
    ]

findVariables :: LexingError e => SP e Char (Lexeme Token)
findVariables = lexerSP lexicalSpec

stripName = dropAround (=='$')

doSubst :: M.Map Text Text
        -> Lexeme Token
        -> Text
doSubst s (Lexeme Text     _ txt) = txt
doSubst s (Lexeme Variable _ varname) =
    case M.lookup (stripName varname) s of
      Nothing -> varname
      Just t  -> t

applyVariableSubstitution :: (UTF8DecodeError e, LexingError e) =>
                             [(Text, Text)]
                          -> SP e ByteString ByteString
applyVariableSubstitution subst =
    toWord8 >>>
    decodeUTF8 >>>
    findVariables >>>
    mapSP (doSubst $ M.fromList subst) >>>
    mapSP encodeUtf8
