{-# LANGUAGE RankNTypes, GADTs #-}

module Language.Forvie.Grammar.ActiveRHS
    ( RHS (..)
    , Grammar
    , token
    )
    where

import Control.Applicative
import Control.Monad (ap)

data RHS nt tok v a where
    Return  :: a ->                                RHS nt tok v a
    Choice  :: RHS nt tok v a -> RHS nt tok v a -> RHS nt tok v a
    Failure :: String                           -> RHS nt tok v a
    Token   :: (tok -> RHS nt tok v a) ->          RHS nt tok v a
    NT      :: nt b -> (v b -> RHS nt tok v a)  -> RHS nt tok v a

type Grammar nt tok f = forall v a. nt a -> RHS nt tok v (f v a)

token :: (Show tok, Eq tok) => tok -> RHS nt tok v a -> RHS nt tok v a
token tok rhs =
    Token $ \tok' -> if tok == tok' then rhs
                     else (Failure $ "expecting " ++ show tok)

instance Monad (RHS nt tok v) where
    return = Return
    Return a         >>= f = f a
    Choice rhs1 rhs2 >>= f = Choice (rhs1 >>= f) (rhs2 >>= f)
    Failure message  >>= f = Failure message
    Token k          >>= f = Token (\token -> k token >>= f)
    NT nt k          >>= f = NT nt (\v -> k v >>= f)

instance Functor (RHS nt tok v) where
    fmap f rhs = rhs >>= return . f

instance Applicative (RHS nt tok v) where
    pure = return
    (<*>) = ap