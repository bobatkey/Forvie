{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}

module Language.Forvie.Parsing.Grammar where

import Control.Monad (ap)
import Control.Applicative

import Data.Text (Text)
import Data.Type.Equality
import Data.Type.Eq
import Data.Type.Show

data Call nt b where
    Call :: (Ord a, Show a) => nt a b -> a -> Int -> Call nt b

instance Show3 nt => Show2 (Call nt) where
    show2 (Call nt a l) = "(Call " ++ show3 nt ++ " " ++ show a ++ " " ++ show l ++ ")"

instance Eq3 nt => Eq2 (Call nt) where
    Call nt1 a1 l1 === Call nt2 a2 l2 = 
        case nt1 ==== nt2 of
          Just (Refl,Refl) -> if a1 == a2 && l1 == l2 then Just Refl else Nothing
          Nothing          -> Nothing

data Component nt tok v a where
    Accept  :: a ->
               Component nt tok v a
    WfToken :: tok ->
               (Text -> RHS nt tok v a) ->
               Component nt tok v a
    WfCall  :: Bool ->
               Call nt b ->
               (v b -> RHS nt tok v a) ->
               Component nt tok v a

newtype RHS nt tok v a = RHS { components :: [Component nt tok v a] }

instance Monad (RHS nt tok v) where
    return a = RHS [Accept a]
    RHS l >>= f = RHS $ l >>= bindComponent f

bindComponent :: (a -> RHS nt tok v b) -> Component nt tok v a -> [Component nt tok v b]
bindComponent f (Accept a)        = components (f a)
bindComponent f (WfToken tok k)   = [WfToken tok (\t -> k t >>= f)]
bindComponent f (WfCall b call k) = [WfCall b call (\v -> k v >>= f)]

instance Functor (RHS nt tok v) where
    fmap = liftA

instance Applicative (RHS nt tok v) where
    pure = return
    (<*>) = ap

instance Alternative (RHS nt tok v) where
    empty = RHS []
    RHS x <|> RHS y = RHS (x ++ y)

list m = return [] <|> (:) <$> m <*> list m

nonEmptyList p = (:) <$> p <*> list p


terminal :: tok -> RHS nt tok v Text
terminal tok = RHS [WfToken tok $ \t -> RHS [Accept t]]

noPrec :: RHS nt tok v (f v b) -> Int -> () -> RHS nt tok v (f v b)
noPrec rhs l () = rhs

atLevel :: Int -> RHS nt tok v (f v b) -> Int -> () -> RHS nt tok v (f v b)
atLevel l rhs l' () = if l == l' then rhs else RHS []

callAt :: nt () b -> Int -> RHS nt tok v (v b)
callAt nt l = RHS [WfCall False (Call nt () l) $ \v -> RHS [Accept v]]

callTop nt = callAt nt 4

call :: nt () b -> RHS nt tok v (v b)
call nt = RHS [WfCall False (Call nt () 0) $ \v -> RHS [Accept v]]

--------------------------------------------------------------------------------
type Grammar f nt tok =
    forall a b (v :: * -> *). nt a b -> Int -> a -> RHS nt tok v (f v b)

getRHS :: Grammar f nt tok
       -> Call nt b
       -> RHS nt tok v (f v b)
getRHS grammar (Call nt a l) = grammar nt l a
