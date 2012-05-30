{-# LANGUAGE OverloadedStrings #-}

module Layout
    ( prop_layout )
    where

import Prelude hiding (map)
import Data.List (intersperse)
import Data.MonadicStream hiding (concatMap)
import Control.Applicative
import Test.QuickCheck
import Text.Position
import Text.Lexeme
import qualified Data.Set as S
import Language.Forvie.Layout

-- This module defines QuickCheck2 property that states that, for a
-- given s-expression, a mixed layout/non-layout regime gives the same
-- answer as strictly inserted non-layout delimiters.

-- This test is not a complete specification of layout...

-- FIXME: also remember to generate some examples with {;} already in

--------------------------------------------------------------------------------
data Structure
    = Block Bool [Structure] -- 
    | Atom
    deriving Show

instance Arbitrary Structure where
    arbitrary = sized structure
        where
          structure 0 =
              pure Atom
          structure n =
              oneof [ pure Atom
                    , Block <$> arbitrary <*> structures n ]

          structures 0 = pure []
          structures n =
              oneof [ pure []
                    , (:) <$> structure (n `div` 2) <*> structures (n `div` 2)
                    ]

data SToken
    = TokBlock
    | TokAtom
    | TokLBrace
    | TokRBrace
    | TokSemicolon
    deriving (Eq, Show, Ord)

instance Layout SToken where
    semicolon = TokSemicolon
    lbrace    = TokLBrace
    rbrace    = TokRBrace
    blockOpener = S.fromList [TokBlock]

--------------------------------------------------------------------------------
-- makes a dummy lexeme with the given indentation and token
-- FIXME: do the indentation and line counts correctly
mkLexeme :: Int -> a -> Lexeme a
mkLexeme indent a = Lexeme a (Span (Position 0 0 indent) (Position 0 0 indent)) ""

--------------------------------------------------------------------------------
-- Takes a structure and renders it as a stream of tokens, with the
-- indentation levels and newlines inserted to simulate a user using
-- layout to denote block structure.
renderWithLayout :: Int -- indentation level
                 -> Structure
                 -> [Lexeme (NewlineOr SToken)]
renderWithLayout indent (Block True elements) =
    [ mkLexeme indent (Token TokBlock)
    , mkLexeme indent Newline ]
    ++ renderListWithLayout (indent+1) elements
renderWithLayout indent (Block False elements) =
    [ mkLexeme indent (Token TokBlock)
    , mkLexeme indent (Token TokLBrace) ]
    ++ Prelude.concat (intersperse [mkLexeme indent (Token TokSemicolon)] $ fmap (renderWithLayout indent) elements)
    ++ [ mkLexeme indent (Token TokRBrace)
       , mkLexeme indent Newline ]
renderWithLayout indent Atom =
    [ mkLexeme indent (Token TokAtom)
    , mkLexeme indent Newline
    ]

renderListWithLayout :: Int
                     -> [Structure]
                     -> [Lexeme (NewlineOr SToken)]
renderListWithLayout indent =
    concatMap (renderWithLayout indent)

--------------------------------------------------------------------------------
-- FIXME: get render without layout to return Maybe, and return
-- Nothing when the layout usage annotations are invalid (not allowed
-- to have empty layout blocks inside delimited blocks)
renderWithoutLayout :: Structure
                    -> [SToken]
renderWithoutLayout (Block _ elements) =
    TokBlock : renderListWithoutLayout elements
renderWithoutLayout Atom =
    [TokAtom]

renderListWithoutLayout :: [Structure]
                        -> [SToken]
renderListWithoutLayout elements =
    TokLBrace : Prelude.concat (intersperse [TokSemicolon] $ fmap renderWithoutLayout elements) ++ [TokRBrace]

--------------------------------------------------------------------------------
-- should this parse?
-- block { block }

-- block { block
--           atom; atom }

--   What does this mean?
-- block { block
-- ; atom
-- }

-- GHC seems to treat the "; atom" as part of the inner block. And the
-- final '}' closes all blocks. This is what the current
-- implementation in forvie does. I think that renderWithLayout needs
-- to generate the right thing in this case.

-- block
--  block
--  atom

-- what is the actual specification of layout?
-- - exactly what are the rules for empty blocks?
-- - what are the rules for mixing user-delimited blocks and layout blocks?
-- - exactly what information does the insertion of layout information rely on?

-- block
--  block
--   atom
--  block { atom; block { atom; atom }; block }


--------------------------------------------------------------------------------
-- checkStructure ensures that empty layout blocks inside
-- user-delimited blocks never have anything after them. This is not
-- representable using mixed layout/non-layout
checkStructure :: Structure -> Bool
checkStructure (Block False l) = checkList l
    where
      checkList []                  = True
      checkList (Block True []:_:l) = False
      checkList (x:l)               = checkStructure x && checkList l
checkStructure (Block True l) =
    all checkStructure l
checkStructure (Atom) =
    True

--------------------------------------------------------------------------------
structureLayout :: [Structure] -> Maybe [SToken]
structureLayout s = ofList l |>> layout (OnLayoutError $ const Nothing) |>> map lexemeTok |>| toList
    where l = renderListWithLayout 0 s

--------------------------------------------------------------------------------
prop_layout :: [Structure] -> Property
prop_layout s =
    all checkStructure s ==>
    case structureLayout s of
      Nothing -> False
      Just l' -> renderListWithoutLayout s == l'
