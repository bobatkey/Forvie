{-# LANGUAGE ExistentialQuantification #-}

module Language.Forvie.Parsing.Monad
    ( P
    , nt
    , terminal
    , list
    , nonEmptyList
    , PrecLevel
    , reset
    , atLevel
    , down
    , nt'
    , setLevel
    )
    where

import Data.Text (Text)
import Control.Applicative
import Control.Monad (ap)
import Language.Forvie.Parsing.Grammar

data MonadRHS nt tok v a
    = Return a
    | Zero
    | Plus (MonadRHS nt tok v a) (MonadRHS nt tok v a)
    | Token tok (Text -> MonadRHS nt tok v a)
    | forall x y. (Eq x, Show x) => NonTerminal (nt x y) x (v y -> MonadRHS nt tok v a)

instance Monad (MonadRHS nt tok v) where
    return = Return
    Return a           >>= f = f a
    Zero               >>= f = Zero
    Plus x y           >>= f = Plus (x >>= f) (y >>= f)
    Token tok k        >>= f = Token tok (\t -> k t >>= f)
    NonTerminal nt x k >>= f = NonTerminal nt x (\y -> k y >>= f)

instance Functor (MonadRHS nt tok v) where
    fmap = liftA

instance Applicative (MonadRHS nt tok v) where
    pure = return
    (<*>) = ap

instance Alternative (MonadRHS nt tok v) where
    empty = Zero
    (<|>) = Plus

--------------------------------------------------------------------------------
newtype P nt tok v a b = P { unP :: a -> MonadRHS nt tok v b }

instance Functor (P nt tok v a) where
    fmap f (P k) = P $ \a -> fmap f (k a)

instance Applicative (P nt tok v a) where
    pure x = P $ \_ -> pure x
    P f <*> P x = P $ \a -> f a <*> x a

instance Alternative (P nt tok v a) where
    empty = P $ \_ -> empty
    P x <|> P y = P $ \a -> x a <|> y a

instance RHS P where
    ($$) (P k) a = P $ \() -> k a
    components (P k) = components' (k ())

components' (Return a)           = [Accept a]
components' Zero                 = []
components' (Plus x y)           = components' x ++ components' y
components' (Token tok y)        = [WfToken tok (P y)]
components' (NonTerminal nt x y) = [WfCall False (Call nt x) (P y)]

--------------------------------------------------------------------------------
nt :: (Show a, Eq a) => nt a b -> P nt tok v a (v b)
nt s = P $ \a -> NonTerminal s a Return

nt' :: nt () b -> P nt tok v a (v b)
nt' s = P $ \a -> NonTerminal s () Return

terminal :: tok -> P nt tok v a Text
terminal tok = P $ \a -> Token tok Return

--------------------------------------------------------------------------------
-- 'm' needs to actually advance in the input for this to be ok.
list0 :: MonadRHS nt tok v a -> MonadRHS nt tok v [a]
list0 m = return [] <|> (:) <$> m <*> list0 m

list :: P nt tok v a b -> P nt tok v a [b]
list (P k) = P $ \a -> list0 (k a)

nonEmptyList :: P nt tok v a b -> P nt tok v a [b]
nonEmptyList p = (:) <$> p <*> list p

--------------------------------------------------------------------------------
newtype PrecLevel = PL Int
    deriving (Eq, Show)

atLevel :: Int -> P nt tok v PrecLevel a -> P nt tok v PrecLevel a
atLevel i (P k) = P $ \(PL l) -> if i <= l then k (PL i) else Zero

down :: P nt tok v PrecLevel a -> P nt tok v PrecLevel a
down (P k) = P $ \(PL l) -> k (PL (l - 1))

setLevel :: Int -> P nt tok v PrecLevel a -> P nt tok v x a
setLevel l (P k) = P $ \_ -> k (PL l)

reset = setLevel 10
