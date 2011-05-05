{-# LANGUAGE ExistentialQuantification, Arrows #-}

module Language.Forvie.Parsing.Arrow
    ( ArrowRHS
    , nt
    , terminal
    , check
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

import Prelude hiding (id, (.))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Type.Show
import Control.Category
import Control.Applicative
import Control.Arrow
import Language.Forvie.Parsing.Grammar

assoc (x,(y,z)) = ((x,y),z)
associnv ((x,y),z) = (x,(y,z))

data ArrowRHS nt tok v a b
    = Arr (a -> b)
    | Zero
    | Plus (ArrowRHS nt tok v a b) (ArrowRHS nt tok v a b)
    | forall x. Token tok (ArrowRHS nt tok v (Text, a) b)
    | forall x y c. (Eq x, Show x) => NonTerminal (a -> (x,c)) (nt x y) (ArrowRHS nt tok v (v y, c) b)
    | forall c. Check (a -> (Bool,c)) (ArrowRHS nt tok v c b)
    | forall s c. Star (a -> (s,c)) (ArrowRHS nt tok v s s) (ArrowRHS nt tok v (s,c) b)

instance (Show tok, Show3 nt) => Show (ArrowRHS nt tok v a b) where
    show (Arr f)              = "Arr"
    show Zero                 = "Zero"
    show (Plus p q)           = "(Plus " ++ show p ++ " " ++ show q ++ ")"
    show (Token cs p)         = "(Token " ++ show cs ++ " " ++ show p ++ ")"
    show (NonTerminal f nt p) = "(NonTerminal " ++ show3 nt ++ " " ++ show p ++ ")"
    show (Check _ p)          = "(Check " ++ show p ++ ")"
    show (Star _ p q)         = "(Star " ++ show p ++ " " ++ show q ++ ")"

--------------------------------------------------------------------------------
terminal :: tok -> ArrowRHS nt tok v x Text
terminal t = Token t (Arr fst)

nt :: (Show a, Eq a) => nt a b -> ArrowRHS nt tok v a (v b)
nt n = NonTerminal (\a -> (a,())) n (Arr (\(v,()) -> v))

check :: ArrowRHS nt tok v Bool ()
check = Check (\b -> (b,())) (Arr id)

star :: ArrowRHS nt tok v a a -> ArrowRHS nt tok v a a
star r = Star (\a -> (a,())) r (Arr fst)

list :: ArrowRHS nt tok v x a -> ArrowRHS nt tok v x [a]
list r = proc x -> do
           (_,l) <- star (proc (x,l) -> do
                            a <- r -< x
                            returnA -< (x,a:l)) -< (x,[])
           returnA -< reverse l

nonEmptyList :: ArrowRHS nt tok v x a -> ArrowRHS nt tok v x [a]
nonEmptyList r = proc x -> do
                   a <- r -< x
                   l <- list r -< x
                   returnA -< (a:l)

--------------------------------------------------------------------------------
precomp :: (a -> b) -> ArrowRHS nt tok v b c -> ArrowRHS nt tok v a c
precomp f (Arr g)      = Arr (f >>> g)
precomp f Zero         = Zero
precomp f (Plus p q)   = Plus (f `precomp` p)
                              (f `precomp` q)
precomp f (Token cs p) = Token cs (second f `precomp` p)
precomp f (NonTerminal h nt p) = NonTerminal (f >>> h) nt p
precomp f (Check h p)  = Check (f >>> h) p
precomp f (Star h p q)  = Star (f >>> h) p q

instance Category (ArrowRHS nt tok v) where
  id = Arr id
  g . Arr f      = precomp f g
  g . Zero       = Zero
  g . Plus p q   = Plus (g . p) (g . q)
  g . Token cs p = Token cs (g . p)
  g . NonTerminal nt x p = NonTerminal nt x (g . p)
  g . Check h p  = Check h (g . p)
  g . Star h p q  = Star h p (g . q)

instance Arrow (ArrowRHS nt tok v) where
    arr = Arr
    first (Arr f)      = Arr (first f)
    first Zero         = Zero
    first (Plus p q)   = Plus (first p) (first q)
    first (Token cs p) = Token cs (precomp assoc (first p))
    first (NonTerminal h nt p) = NonTerminal (first h >>> associnv) nt (arr assoc >>> first p)
    first (Check h p)  = Check (first h >>> associnv) (first p)
    first (Star h p q)  = Star (first h >>> associnv) p (arr assoc >>> first q)

instance ArrowZero (ArrowRHS nt tok v) where
    zeroArrow = Zero

instance ArrowPlus (ArrowRHS nt tok v) where
    (<+>) = Plus

--------------------------------------------------------------------------------
instance Functor (ArrowRHS nt tok v a) where
    fmap f x = x >>> arr f

instance Applicative (ArrowRHS nt tok v a) where
    pure    = arr . const
    f <*> a = f &&& a >>^ uncurry ($)

instance Alternative (ArrowRHS nt tok v a) where
    empty = zeroArrow
    (<|>) = (<+>)

--------------------------------------------------------------------------------
instance RHS ArrowRHS where
    rhs $$ b = rhs . arr (const b)
    components (Arr f)      = [Accept (f ())]
    components Zero         = []
    components (Plus p q)   = components p ++ components q
    components (Token cs p) = [WfToken cs (arr (\c -> (c,())) >>> p)]
    components (NonTerminal h nt p) = [WfCall False (Call nt x) (arr (\v -> (v,c)) >>> p)]
        where (x,c) = h ()
    components (Check h p)  = if b then components (p $$ a) else []
        where (b,a) = h ()
    components (Star h p q) =
        components (q $$ (s,c))
        ++
        mapMaybe (lfstar c p q) (components (p $$ s))
        where (s,c) = h ()

-- FIXME: make this the same as the one in ArrowRegular
lfstar :: c
       -> ArrowRHS nt tok v s s
       -> ArrowRHS nt tok v (s,c) b
       -> Component nt tok ArrowRHS v s
       -> Maybe (Component nt tok ArrowRHS v b)
lfstar c p q (Accept a) =
    Nothing
lfstar c p q (WfToken cs r) =
    Just $ WfToken cs (r >>> Star (\s -> (s,c)) p q)
lfstar c p q (WfCall _ nt r) =
    Just $ WfCall True nt (r >>> Star (\s -> (s,c)) p q)

--------------------------------------------------------------------------------
-- FIXME: move this elsewhere
data PrecLevel = P !Int
               deriving (Eq, Show)

atLevel :: Int -> ArrowRHS nt tok v PrecLevel a -> ArrowRHS nt tok v PrecLevel a
atLevel i p = proc (P l) -> do
                check -< (i <= l)
                p -< P i

down :: ArrowRHS nt tok v PrecLevel a -> ArrowRHS nt tok v PrecLevel a
down rhs = arr (\(P p) -> P (p-1)) >>> rhs

setLevel :: Int -> ArrowRHS nt tok v PrecLevel b -> ArrowRHS nt tok v a b
setLevel l rhs = arr (const (P l)) >>> rhs

reset = setLevel 10

nt' :: nt () b -> ArrowRHS nt tok v a (v b)
nt' nonterminal = arr (const ()) >>> nt nonterminal

