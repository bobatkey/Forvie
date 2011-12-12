{-# LANGUAGE GADTs, RankNTypes #-}

-- | Representation of context free grammars.
--
-- Why are these context-free, even though we appear to be able to
-- discriminate based on the result of referencing a non-terminal?
-- Because 'v' is abstract, the grammar cannot change its behaviour
-- based on this.
--
-- These grammars are able to construct their own parse results. The
-- parsing machinery ensures that this is done efficiently. We could
-- also go back to full-on context free grammars that do not produce
-- parse results (or do so in a generic way).
--
-- There is no easy way of making sure that the set of non-terminals
-- is finite, I think.

module Language.Forvie.Grammar.ContextFree
    ( RHS (..)
    , Grammar
    )
    where

data RHS nt tok v a where
    Return  :: a                                -> RHS nt tok v a
    Choice  :: RHS nt tok v a -> RHS nt tok v a -> RHS nt tok v a
    Failure ::                                     RHS nt tok v a
    Token   :: (tok -> Bool)  -> RHS nt tok v a -> RHS nt tok v a
    NT      :: nt b -> (v b -> RHS nt tok v a)  -> RHS nt tok v a

type Grammar nt tok f = forall v a. nt a -> RHS nt tok v (f v a)

