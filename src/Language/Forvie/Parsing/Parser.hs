{-# LANGUAGE RankNTypes, GADTs, KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, TypeFamilies, UndecidableInstances #-}

module Language.Forvie.Parsing.Parser where

import qualified Data.IntMap as IM
import qualified Data.Set as S
import           Data.Char
import           Control.Monad (unless)
import           Control.Monad.State
import           Control.Monad.ST
import           Data.STRef
import           Data.TypedMap (Equal (..), Compare1 (..), Ord1 (..), Eq1 (..), P (..))
import qualified Data.TypedMap as TM

-- TODO:
-- - better data structures for storing (pos,nt)->a maps
-- - tail calls
-- - kleene star? by translation into tail calls
-- - return values
-- - gc-able representation of previous sets
--   - at the moment, we hang on to a reference (i,nt) even if no one will ever respond to it
-- - error reporting (needs token sets on Token)
-- - integration with MonadicStream stuff
-- - generation of SPPFs, and resurrection of the graph drawing stuff
-- - derivation of Eq1, Show1 and Ord1 for non-terminals (is this possible yet)
-- - doing it in a dependently typed language
-- - stratified results; which would probably make the storage of waiting sets easier (maybe)

{------------------------------------------------------------------------------}
-- Auxillary stuff
class Show1 f where
    show1 :: f a -> String

{------------------------------------------------------------------------------}
-- Step 1: Grammars
data RHS nt tok v a where
    Return  :: a ->                                RHS nt tok v a
    Choice  :: RHS nt tok v a -> RHS nt tok v a -> RHS nt tok v a
    Failure ::                                     RHS nt tok v a
    Token   :: (tok -> RHS nt tok v a) ->          RHS nt tok v a
    NT      :: nt b -> (v b -> RHS nt tok v a)  -> RHS nt tok v a

type Grammar nt tok f = forall v a. nt a -> RHS nt tok v (f v a)

char :: Char -> RHS nt Char v a -> RHS nt Char v a
char c rhs = Token $ \c' -> if c == c' then rhs else Failure

{------------------------------------------------------------------------------}
-- an example
data Expr
data ExprList

data AST v a where
    Atom  :: Char                 -> AST v Expr
    Paren :: v ExprList           -> AST v Expr
    Emp   ::                         AST v ExprList
    Cons  :: v Expr -> v ExprList -> AST v ExprList

instance Show (AST v a) where
    show (Atom c)   = "(Atom " ++ [c] ++ ")"
    show (Paren _)  = "(Paren ?)"
    show (Emp)      = "Emp"
    show (Cons _ _) = "(Cons ? ?)"

instance Show1 (AST v) where
    show1 x = show x

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
    Choice (Token $ \c -> if isAlpha c then Return (Atom c) else Failure)
           (char '(' $ NT ExprList $ \e -> char ')' $ Return (Paren e))
grammar ExprList =
    Choice (Return Emp)
           (NT Expr $ \e -> NT ExprList $ \es -> Return (Cons e es))
{-
-- NT needs to depend on 'v'

-- and 'v' needs deciable equality: bugger. Is there any way round
-- this? Perhaps by having special non-terminals that can be called
-- tail-recursively, and ignoring the recursion checks.

-- Is 'v' having to have deciable equality so bad? Don't think so, the
-- proofs would go through with it, I think.

-- What if you write:
--   NT X $ \vx -> NT X $ \vy -> if vx == vy then Failure else (Pair vx vy)
-- What on earth does that mean? I think the paper precludes this using the parametricity constraint

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


data Knot f a = In (f (Knot f) a)

pr :: Knot AST a -> String
pr (In (Atom c))    = [c]
pr (In (Paren e))   = "(" ++ pr e ++ ")"
pr (In Emp)         = ""
pr (In (Cons e es)) = pr e ++ pr es

{-
instance ParseResultsMonad f Maybe where
    type ResultNode f Maybe = Knot f
    newResult i j x = Just (In x)
    addResult v x   = Nothing
-}

data K a b = K a

-- This demonstrates that the parser does a lot of unnecessary work.
{-
instance Show1 (f (K ())) => ParseResultsMonad f IO where
    type ResultNode f IO = K ()
    newResult i j x = do putStrLn $ "New result from " ++ show i ++ " to " ++ show j ++ ": " ++ show1 x
                         return (K ())
    addResult v x   = return ()
-}
{------------------------------------------------------------------------------}
-- building actual imperative graphs of the parse results
instance ParseResultsMonad f (ST s) where
    data ResultNode f (ST s) a = STRN (STRef s [f (ResultNode f (ST s)) a])
    newResult i j x      = do v <- newSTRef [x]; return (STRN v)
    addResult (STRN v) x = modifySTRef v (x:)

    data StackNode (ST s) a = STSN (STRef s [a])
    newStackNode a            = do v <- newSTRef [a]; return (STSN v)
    addToStackNode a (STSN v) = modifySTRef v (a:)
    readStackNode (STSN v)    = readSTRef v

-- TODO: version with IORefs
--       version that rejects ambiguity, but uses refs to store the waiting graph
--       purely functional version (without GC), with different ambiguity rejection behaviour
--       add a 

{------------------------------------------------------------------------------}
-- Step 2: Parsing (naively)

-- TODO:
-- - experiment with the CPS version to see whether it is faster
-- - error reporting

{----------------------------------------}
class Monad m => ParseResultsMonad f m where
    data ResultNode f m :: * -> *
    newResult :: Int -> Int -> f (ResultNode f m) x -> m (ResultNode f m x)
    addResult :: ResultNode f m x -> f (ResultNode f m) x -> m ()

    data StackNode m :: * -> *
    newStackNode    :: a -> m (StackNode m a)
    addToStackNode  :: a -> StackNode m a -> m ()
    readStackNode   :: StackNode m a -> m [a]

{----------------------------------------}
parse :: (Ord1 nt, ParseResultsMonad f m) =>
         Grammar nt tok f
      -> nt a
      -> [tok]
      -> m (Maybe (ResultNode f m a))
parse grammar nt input =
    go [Item (grammar nt) TopLevel] IM.empty 0 input
    where
      go items previous j [] = do
        (_, _, complete) <- executeAll grammar Nothing j items previous
        return complete
      go items previous j (tok:toks) = do
        (previous', newItems, _) <- executeAll grammar (Just tok) j items previous
        go newItems previous' (j+1) toks

-- If newItems is empty (or only contains “Failure”), then we have
-- encountered a parse error. Have a look at the 'items' list to
-- derive a stack-trace of what went wrong. We can also tag “Failure”
-- with an error-message to describe the fault.

{----------------------------------------}
-- Auxillary data structures
data ReturnAddress nt t b where
    TopLevel :: ReturnAddress nt t t
    Previous :: Int -> nt b -> ReturnAddress nt t b

data Item nt tok f m t where
    Item :: RHS nt tok (ResultNode f m) (f (ResultNode f m) b) ->
            ReturnAddress nt t b ->
            Item nt tok f m t

data WaitingForCall nt tok f m t where
    WfCall :: nt a
           -> (ResultNode f m a -> RHS nt tok (ResultNode f m) (f (ResultNode f m) b))
           -> ReturnAddress nt t b
           -> WaitingForCall nt tok f m t

-- TODO: return addresses need to refer to StackNode f m (WaitingForCall nt tok f m t b)
--       how to lower the amount of circularity?

type Waiting nt tok f m t =
    IM.IntMap [WaitingForCall nt tok f m t]

{--------------------}
-- Move this lot elsewhere (Data.TypedSet?)
data SomeNT nt where
    SomeNT :: nt a -> SomeNT nt

instance Eq1 nt => Eq (SomeNT nt) where
    SomeNT nt1 == SomeNT nt2 =
        case nt1 === nt2 of Nothing -> False; Just _ -> True

instance Ord1 nt => Ord (SomeNT nt) where
    compare (SomeNT nt1) (SomeNT nt2) =
        case compare1 nt1 nt2 of LT1 -> LT; EQ1 -> EQ; GT1 -> GT

{----------------------------------------}
data ParserState nt tok f m t
    = ParserState { known      :: TM.Map (P Int nt) (ResultNode f m)     -- ^ complete sub-parses ending at this point
                  , complete   :: Maybe (ResultNode f m t)             -- ^ reference to the complete parses to this point
                  , called     :: S.Set (SomeNT nt)       -- ^ calls that have been made here
                  , waiting    :: Waiting nt tok f m t    -- ^ suspended items waiting for completion of a call
                  , continuing :: [Item nt tok f m t]     -- ^ items that will continue to the next step
                  }

-- waiting :: TM.Map (P Int nt) (WaitingItems nt tok f v t)

-- so this describes a graph, which needs to be garbage collected
-- occasionally.

-- the executeAll function below essentially treats “waiting” as a
-- piece of state. So we should push it into the ParseResultsMonad,
-- and have two sorts of variables. The “waiting” field then becomes a
-- map of return addresses to references to suspended items.

initState :: Waiting nt tok f m t -> ParserState nt tok f m t
initState waitingSet =
    ParserState { known      = TM.empty
                , complete   = Nothing
                , called     = S.empty
                , waiting    = waitingSet
                , continuing = []
                }

{----------------------------------------}
type M nt tok f t m a = StateT (ParserState nt tok f m t) m a

addComplete :: ParseResultsMonad f m => Int -> f (ResultNode f m) t -> M nt tok f t m ()
addComplete j x = do
  v <- gets complete
  case v of
    Nothing -> do v <- lift $ newResult 0 j x
                  modify $ \s -> s { complete = Just v }
    Just v  -> do lift $ addResult v x

checkKnown :: (Eq1 nt, Monad m) => Int -> nt a -> M nt tok f t m (Maybe (ResultNode f m a))
checkKnown i nt = gets (TM.lookup (P i nt) . known)

addKnown :: Monad m => Int -> nt a -> ResultNode f m a -> M nt tok f t m ()
addKnown i nt v = modify $ \s -> s { known = TM.insert (P i nt) v (known s) }

addContinuing :: Monad m => Item nt tok f m t -> M nt tok f t m ()
addContinuing item = modify $ \s -> s { continuing = item : continuing s }

addWaitingForCall :: Monad m => Int -> WaitingForCall nt tok f m t -> M nt tok f t m ()
addWaitingForCall j wfcall = do
  modify $ \s -> s { waiting = IM.alter (maybe (Just [wfcall]) (Just . (wfcall:))) j (waiting s) }

getWaiting :: (Eq1 nt, Monad m) => Int -> nt a -> ResultNode f m a -> M nt tok f t m [Item nt tok f m t]
getWaiting i nt v = do
  set <- gets (maybe [] id . IM.lookup i . waiting)
  return (findNT set)
    where
      findNT [] = []
      findNT (WfCall nt' k returnAddress:wfs) =
          case nt === nt' of
            Nothing   -> findNT wfs
            Just Refl -> Item (k v) returnAddress : findNT wfs

recordCall :: (Ord1 nt, Monad m) => nt a -> M nt tok f t m Bool
recordCall nt = do
  beenCalled <- gets called
  case SomeNT nt `S.member` beenCalled of
    True  -> return True
    False -> do
      modify $ \s -> s { called = S.insert (SomeNT nt) (called s) }
      return False

{----------------------------------------}
executeAll :: (Ord1 nt, ParseResultsMonad f m) =>
              Grammar nt tok f     -- ^ The grammar
           -> Maybe tok            -- ^ Current token
           -> Int                  -- ^ The current position
           -> [Item nt tok f m t]  -- ^ Items to be processed
           -> Waiting nt tok f m t -- ^ Waiting items
           -> m (Waiting nt tok f m t,
                 [Item nt tok f m t],
                 Maybe (ResultNode f m t))      -- ^ The new waiting items set, the new items, and complete parses to this point
executeAll grammar token j items previous = do
  st <- execStateT (mapM_ execute items) (initState previous)
  return (waiting st, continuing st, complete st)
    where
      execute (Item (Return a) TopLevel) = do
        addComplete j a

      execute (Item (Return a) (Previous i nt)) = do
        known <- checkKnown i nt
        case known of
          Nothing -> do
            v <- lift $ newResult i j a
            addKnown i nt v
            mapM_ execute =<< getWaiting i nt v
          Just v  -> do
            lift $ addResult v a

      execute (Item Failure returnAddress) = do
        return ()

      execute (Item (Choice rhs1 rhs2) returnAddress) = do
        execute (Item rhs1 returnAddress)
        execute (Item rhs2 returnAddress)

      execute (Item (Token k) returnAddress) = do
        case token of
          Nothing -> return ()
          Just t  -> addContinuing (Item (k t) returnAddress)

      execute (Item (NT nt k) returnAddress) = do
        addWaitingForCall j (WfCall nt k returnAddress)
        known <- checkKnown j nt
        case known of
          Just v  -> do
            execute (Item (k v) returnAddress)
          Nothing -> do
            alreadyCalled <- recordCall nt
            unless alreadyCalled $ execute (Item (grammar nt) (Previous j nt))
{-
      execute (Item (TailCall nt) returnAddress) = do
        alreadyCalled <- recordTailCall nt returnAddress
        unless alreadyCalled $ execute (Item (grammar nt) returnAddress)
-}

-- If you have an LL grammar, then this algorithm should be quite
-- fast...? If you have an LR(1) grammar then it should also be quite
-- fast? If you use tail calls?

-- addWaitingForCall should just return a reference that is unique for
-- that pair of (j,nt). That reference then refers to the actual list
-- of waiting things. 

-- So “Previous :: Int -> nt b -> Ref b -> ReturnAddress nt t b

-- where Ref a = STRef s [WaitingForCall nt tok f v t a]
--  and WfCall :: (v a -> RHS nt tok v (f v b)) -> ReturnAddress nt t b -> WaitingForCall nt tok f v t a

-- so that there is a graph-structured stack of pending items stored
-- on the heap.

-- In the local state, we keep a map (pos,nt)->references

-- The advantage of this setup is that the Haskell run-time can
-- dispose of references when no current item refers to them. It can
-- do this better than any GC mechanism I can come up with. Problem is
-- that it infects all the types involved. I would need to thread
-- something like the ST monad through everything in order to make it
-- work. Or use 'unsafePerformIO'? Or add it to the parse results
-- monad interface...

-- It is either this, or a homemade mark-and-sweep GC, using the
-- current item set as the roots. This would probably suck. It would
-- work by starting with the current items. For each item, we get its
-- return address and push them all on to a worklist, and record them
-- as “used”. For everything in the worklist, we then look at those
-- items in the waiting set, and add their return addresses to the
-- “used“ set. And carry on. When the worklist is empty, we discard
-- everything that is not “used”.

-- Or attempt to construct the correct object graph in memory... this
-- is what I did last time, and it was a bit messy, and relied on
-- (apparent) circularity to work.
