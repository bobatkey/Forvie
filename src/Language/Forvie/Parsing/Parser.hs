{-# LANGUAGE RankNTypes, GADTs, KindSignatures, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}

module Language.Forvie.Parsing.Parser where

import           Data.Char
import           Control.Monad (unless)
import           Control.Monad.State
import           Control.Monad.ST
import           Data.STRef
import           Data.IORef
import           Data.TypedMap (Equal (..), Compare1 (..), Ord1 (..), Eq1 (..), P (..))
import qualified Data.TypedMap as TM
import qualified Data.TypedSet as TS

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


data Knot f a = In (f (Knot f) a)

pr :: Knot AST a -> String
pr (In (Atom c))    = [c]
pr (In (Paren e))   = "(" ++ pr e ++ ")"
pr (In Emp)         = ""
pr (In (Cons e es)) = pr e ++ pr es

data K a b = K a

{------------------------------------------------------------------------------}
-- building actual imperative graphs of the parse results
data STRN f s a = STRN (STRef s [f (STRN f s) a])

instance ParseResultsMonad f (ST s) where
    type ResultNode f (ST s) = STRN f s
    newResult i j x      = do v <- newSTRef [x]; return (STRN v)
    addResult (STRN v) x = modifySTRef v (x:)

instance ParseStackMonad nt tok f (ST s) where
    data StackNode nt tok f (ST s) t a = STSN (STRef s [WaitingForCall nt tok f (ST s) t a])
    newStackNode a            = do v <- newSTRef [a]; return (STSN v)
    addToStackNode a (STSN v) = modifySTRef v (a:)
    readStackNode (STSN v)    = readSTRef v

--------------------------------------------------
-- This demonstrates that the parser does a lot of unnecessary work,
-- and that tail call optimisation is desirable
instance Show1 (f (K ())) => ParseResultsMonad f IO where
    type ResultNode f IO = K ()
    newResult i j x = do putStrLn $ "New result from " ++ show i ++ " to " ++ show j ++ ": " ++ show1 x
                         return (K ())
    addResult v x   = return ()

{-
instance ParseResultsMonad f IO where
    type ResultNode f IO = Knot f
    newResult i j x      = return (In x)
    addResult _ x        = error "ambiguity detected"
-}

instance Show1 (f (K ())) => ParseStackMonad nt tok f IO where
    data StackNode nt tok f IO t a = IOSN (IORef [WaitingForCall nt tok f IO t a])
    newStackNode a            = do v <- newIORef [a]; return (IOSN v)
    addToStackNode a (IOSN v) = modifyIORef v (a:)
    readStackNode (IOSN v)    = readIORef v

-- TODO: version with IORefs (is there a typeclass that covers IO/STRefs?)
--       version that rejects ambiguity, but uses refs to store the waiting graph
--       purely functional version (without GC), with different ambiguity rejection behaviour
--       add an error reporting whotsit

{------------------------------------------------------------------------------}
-- oops: the top-level result type needs to be a parameter of the type
-- class too.
--newtype PMonad nt tok f a =
--    PM (StateT (Waiting nt tok f t) Maybe a)

-- Plan: use Maybe to report ambiguity errors
--       use the normal state monad to do garbage collection

-- Overall plan:
-- - collect some grammars to test with
--   - with and without tailcalls (or by translation from Kleene star)
-- - collect benchmarks of different parsing strategies
-- - 

{------------------------------------------------------------------------------}
-- TODO:
-- - experiment with the CPS version to see whether it is faster
-- - error reporting

{----------------------------------------}
class Monad m => ParseResultsMonad f m where
    type ResultNode f m :: * -> *
    newResult :: Int -> Int -> f (ResultNode f m) x -> m (ResultNode f m x)
    addResult :: ResultNode f m x -> f (ResultNode f m) x -> m ()

class ParseResultsMonad f m => ParseStackMonad nt tok f m where
    data StackNode nt tok f m :: * -> * -> *
    newStackNode    :: WaitingForCall nt tok f m t a -> m (StackNode nt tok f m t a)
    addToStackNode  :: WaitingForCall nt tok f m t a -> StackNode nt tok f m t a -> m ()
    readStackNode   :: StackNode nt tok f m t a -> m [WaitingForCall nt tok f m t a]

data ReturnAddress nt tok f m t a where
    TopLevel :: ReturnAddress nt tok f m t t
    Previous :: Int -> nt a -> StackNode nt tok f m t a -> ReturnAddress nt tok f m t a

data WaitingForCall nt tok f m t a where
    WfCall :: (ResultNode f m a -> RHS nt tok (ResultNode f m) (f (ResultNode f m) b))
           -> ReturnAddress nt tok f m t b
           -> WaitingForCall nt tok f m t a

data Item nt tok f m t where
    Item :: RHS nt tok (ResultNode f m) (f (ResultNode f m) b)
         -> ReturnAddress nt tok f m t b
         -> Item nt tok f m t

{----------------------------------------}
parse :: (Ord1 nt, ParseStackMonad nt tok f m) =>
         Grammar nt tok f
      -> nt a
      -> [tok]
      -> m (Maybe (ResultNode f m a))
parse grammar nt input =
    go [Item (grammar nt) TopLevel] 0 input
    where
      go items j [] = do
        (_, complete) <- executeAll grammar Nothing j items
        return complete
      go items j (tok:toks) = do
        (newItems, _) <- executeAll grammar (Just tok) j items
        go newItems (j+1) toks

-- If newItems is empty (or only contains “Failure”), then we have
-- encountered a parse error. Have a look at the 'items' list to
-- derive a stack-trace of what went wrong. We can also tag “Failure”
-- with an error-message to describe the fault.

-- FIXME: Instead of distinguishing between TopLevel and Previous, we
-- should just dump everything in "known" and fish out the complete
-- parse results at the end by looking up (P 0 nt). This would require
-- empty StackNodes, but would get rid of the 't's everywhere, and get
-- rid of the ReturnAddress datatype.

{----------------------------------------}
data ParserState nt tok f m t
    = ParserState { known      :: TM.Map (P Int nt) (ResultNode f m)     -- ^ complete sub-parses ending at this point
                  , complete   :: Maybe (ResultNode f m t)               -- ^ reference to the complete parses to this point
                  , called     :: TS.Set nt                              -- ^ calls that have been made here, used to prevent non-productive loops
                  , waiting    :: TM.Map (P Int nt) (StackNode nt tok f m t)
                  , continuing :: [Item nt tok f m t]                    -- ^ items that will continue to the next step
                  }

initState :: ParserState nt tok f m t
initState =
    ParserState { known      = TM.empty
                , complete   = Nothing
                , called     = TS.empty
                , waiting    = TM.empty
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

addWaitingForCall :: (Eq1 nt, ParseStackMonad nt tok f m) =>
                     Int
                  -> nt a
                  -> WaitingForCall nt tok f m t a
                  -> M nt tok f t m (StackNode nt tok f m t a)
addWaitingForCall j nt wfcall = do
  maybeStackNode <- gets (TM.lookup (P j nt) . waiting)
  case maybeStackNode of
    Nothing -> do
      stackNode <- lift $ newStackNode wfcall
      modify $ \s -> s { waiting = TM.insert (P j nt) stackNode (waiting s) }
      return stackNode
    Just stackNode -> do
      lift $ addToStackNode wfcall stackNode
      return stackNode

getWaiting :: ParseStackMonad nt tok f m =>
              StackNode nt tok f m t a
           -> ResultNode f m a
           -> M nt tok f t m [Item nt tok f m t]
getWaiting stackNode x = do
  wfcalls <- lift $ readStackNode stackNode
  return $ map create wfcalls
      where create (WfCall k returnAddress) = Item (k x) returnAddress

recordCall :: (Ord1 nt, Monad m) => nt a -> M nt tok f t m Bool
recordCall nt = do
  beenCalled <- gets called
  case nt `TS.member` beenCalled of
    True  -> return True
    False -> do
      modify $ \s -> s { called = TS.insert nt (called s) }
      return False

{----------------------------------------}
executeAll :: (Ord1 nt, ParseStackMonad nt tok f m) =>
              Grammar nt tok f     -- ^ The grammar
           -> Maybe tok            -- ^ Current token
           -> Int                  -- ^ The current position
           -> [Item nt tok f m t]  -- ^ Items to be processed
           -> m ([Item nt tok f m t],
                 Maybe (ResultNode f m t))      -- ^ The new waiting items set, the new items, and complete parses to this point
executeAll grammar token j items = do
  st <- execStateT (mapM_ execute items) initState
  return (continuing st, complete st)
    where
      execute (Item (Return a) TopLevel) = do
        addComplete j a

      execute (Item (Return a) (Previous i nt stackNode)) = do
        -- FIXME: get the position and non-terminal from the stackNode
        known <- checkKnown i nt
        case known of
          Nothing -> do
            v <- lift $ newResult i j a
            addKnown i nt v
            mapM_ execute =<< getWaiting stackNode v
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
        stackNode <- addWaitingForCall j nt (WfCall k returnAddress)
        known <- checkKnown j nt
        case known of
          Just v  -> do
            execute (Item (k v) returnAddress)
          Nothing -> do
            alreadyCalled <- recordCall nt
            unless alreadyCalled $ execute (Item (grammar nt) (Previous j nt stackNode))
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
