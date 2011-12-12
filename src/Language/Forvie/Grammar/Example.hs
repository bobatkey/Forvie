{-# LANGUAGE GADTs #-}

module Language.Forvie.Grammar.Example where

import           Data.Char (isAlpha)
import           Data.TypedMap (Equal (..), Compare1 (..), Ord1 (..), Eq1 (..), Show1 (..), Show2 (..))
import           Language.Forvie.Grammar.ActiveRHS

{------------------------------------------------------------------------------}
-- an example
data Expr
data ExprList

data AST v a where
    Atom  :: Char                 -> AST v Expr
    Paren :: v ExprList           -> AST v Expr
    Emp   ::                         AST v ExprList
    Cons  :: v Expr -> v ExprList -> AST v ExprList

instance Show1 v => Show (AST v a) where
    show (Atom c)    = "(Atom " ++ [c] ++ ")"
    show (Paren e)   = "(Paren " ++ show1 e ++ ")"
    show (Emp)       = "Emp"
    show (Cons e es) = "(Cons "++show1 e++" "++show1 es++")"

instance Show2 AST where
    show2 = show

data NT a where
    Expr     :: NT Expr
    ExprList :: NT ExprList

instance Eq1 NT where
    Expr     === Expr     = Just Refl
    ExprList === ExprList = Just Refl
    _        === _        = Nothing

instance Ord1 NT where
    compare1 Expr     Expr     = EQ1
    compare1 Expr     ExprList = LT1
    compare1 ExprList ExprList = EQ1
    compare1 ExprList Expr     = GT1

grammar :: Grammar NT Char AST
grammar Expr =
    Choice (Token $ \c -> if isAlpha c then Return (Atom c) else Failure "expecting alphanumeric character")
           (char '(' $ NT ExprList $ \e -> char ')' $ Return (Paren e))
grammar ExprList =
    Choice (Return Emp)
           (NT Expr $ \e -> NT ExprList $ \es -> Return (Cons e es))
{-
-- NT needs to depend on 'v' for tailcalls to be useful

-- and 'v' needs deciable equality: bugger. Is there any way round
-- this? Perhaps by having special non-terminals that can be called
-- tail-recursively, and ignoring the recursion checks.

-- Is 'v' having to have deciable equality so bad? Don't think so, the
-- proofs would go through with it, I think.

-- What if you write:
--   NT X $ \vx -> NT X $ \vy -> if vx == vy then Failure else (Pair vx vy)
-- What on earth does that mean? I think the paper precludes this using the parametricity constraint. We would here too, since the grammar does not know that result nodes have decidable equality

-- To give a semantics to tail-calls, I need to define what is going
-- on for the generation of the type from a grammar. I think I need to
-- generate a big mutually recursive type, with two different kinds of
-- holes: tail-call and non-tail call.

grammar (ExprList l) =
    Choice (Return (Exprs (reverse l)))
           (NT Expr $ \e -> TailCall (ExprList (e:l)))
-}

{-
or

grammar Expr =
    Choice (Token $ \c -> if isAlpha c then Return (Atom c) else Failure)
           (char '(' $ TailCall (ExprList []))
grammar (ExprList l) =
    Choice (char ')' $ Return (SExpr (reverse l)))
           (NT Expr $ \e -> TailCall (ExprList (e:l)))

-- It would be nice if this could be packaged up in some way, to
-- compile away into another representation. I.e. compile Kleene stars
-- into this representation.

-- Probably don't want to represent grammars as functions, really.

-}

-- TailCall :: nt a -> RHS nt tok v a
-- to execute this, we:
--   let rhs  = grammar nt
--       item = Item rhs returnAddress   --- the caller's returnAddress!
--   execute item
-- what if we have already done this? we could just have a table of
-- tail calls that we have already made; containing the non-terminal
-- invoked and the return address

--------------------------------------------------------------------------------
