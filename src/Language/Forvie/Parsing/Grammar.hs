{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}

module Language.Forvie.Parsing.Grammar where

import Data.Text (Text)
import Data.Type.Equality
import Data.Type.Eq
import Data.Type.Show

data Call nt b where
    Call :: (Eq a, Show a) => nt a b -> a -> Call nt b

instance Show3 nt => Show2 (Call nt) where
    show2 (Call nt a) = "(Call " ++ show3 nt ++ " " ++ show a ++ ")"

instance Eq3 nt => Eq2 (Call nt) where
    Call nt1 a1 === Call nt2 a2 = 
        case nt1 ==== nt2 of
          Just (Refl,Refl) -> if a1 == a2 then Just Refl else Nothing
          Nothing          -> Nothing

data Component nt tok rhs v a where
    Accept  :: a ->
               Component nt tok rhs v a
    WfToken :: tok ->
               rhs nt tok v Text a ->
               Component nt tok rhs v a
    WfCall  :: Bool ->
               Call nt b ->
               rhs nt tok v (v b) a ->
               Component nt tok rhs v a

class RHS rhs where
    ($$)       :: rhs nt tok v a b -> a -> rhs nt tok v () b
    components :: rhs nt tok v () a -> [Component nt tok rhs v a]

type Grammar rhs f nt tok =
    forall a b (v :: * -> *). nt a b -> rhs nt tok v a (f v b)

getRHS :: RHS rhs =>
          Grammar rhs f nt tok
       -> Call nt b
       -> rhs nt tok v () (f v b)
getRHS grammar (Call nt a) = grammar nt $$ a
