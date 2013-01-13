{-# LANGUAGE OverloadedStrings #-}

module LayoutHUnit
    ( layoutTestCases )
    where

import           Prelude hiding (lex, filter, map)
import qualified Data.Text as T
import qualified Data.Set as S
import           Data.MonadicStream hiding (concat, concatMap)
import           Language.Forvie.Lexing.Spec
import           Language.Forvie.Lexing.Text
import           Language.Forvie.Layout
import           Text.Lexeme

import           Test.HUnit

--------------------------------------------------------------------------------
data Token
    = Atom
    | Semicolon
    | LBrace
    | RBrace
    | Block
    deriving (Show, Eq, Ord)

instance Layout Token where
    semicolon = Semicolon
    lbrace    = LBrace
    rbrace    = RBrace
    
    blockOpener = S.fromList [Block]

lexicalSpec :: CompiledLexSpec (Action (NewlineOr Token))
lexicalSpec =
    compileLexicalSpecification $
    [ "x"           :==> Emit (Token Atom)
    , ";"           :==> Emit (Token Semicolon)
    , "{"           :==> Emit (Token LBrace)
    , "}"           :==> Emit (Token RBrace)
    , "block"       :==> Emit (Token Block)
    , "\n"          :==> Emit Newline
    , oneOrMore " " :==> Ignore Whitespace
    ]

-- FIXME: why isn't this part of Forvie?
exceptIgnorable :: Monad m => Processor (Lexeme (Action tok)) m (Lexeme tok)
exceptIgnorable = filter f
    where f (Lexeme (Ignore _) _ _) = Nothing
          f (Lexeme (Emit   t) p s) = Just (Lexeme t p s)

lexer :: Monad m => T.Text -> Stream m (Lexeme (Action (NewlineOr Token)))
lexer = lex lexicalSpec (OnError $ \x -> fail (show x)) "<test input>"

doLayout :: Processor (Lexeme (Action (NewlineOr Token))) Maybe (Lexeme Token)
doLayout = exceptIgnorable >>> layout (OnLayoutError $ const Nothing)

--------------------------------------------------------------------------------
testCases :: [(T.Text, Maybe [Token])]
testCases =
    [ ( "x x"
      , Just [LBrace, Atom, Atom, RBrace])

    , ( "x\nx"
      , Just [LBrace, Atom, Semicolon, Atom, RBrace])

    , ( "x\n x"
      , Just [LBrace, Atom, Atom, RBrace])

    , ( "block\n x\n x"
      , Just [LBrace, Block, LBrace, Atom, Semicolon, Atom, RBrace, RBrace])

    , ( "block\n x\nx"
      , Just [LBrace, Block, LBrace, Atom, RBrace, Semicolon, Atom, RBrace])

    , ( "block\n block\n x"
      , Just [LBrace, Block, LBrace, Block, LBrace, RBrace, Semicolon, Atom, RBrace, RBrace])

    , ( "block\n x\n  x"
      , Just [LBrace, Block, LBrace, Atom, Atom, RBrace, RBrace])
{-
    , ( "  block\n x\n x\nx"
      , Just [LBrace, Block, LBrace, Atom, Semicolon, Atom, RBrace, RBrace])
-}
    ]


checkTestCase :: (T.Text, Maybe [Token]) -> Assertion
checkTestCase (input, expectedOutput) =
    assertEqual "layout test case"
                (lexer input |>> doLayout |>> map lexemeTok |>| toList)
                expectedOutput

layoutTestCases :: Assertion
layoutTestCases = Prelude.mapM_ checkTestCase testCases
