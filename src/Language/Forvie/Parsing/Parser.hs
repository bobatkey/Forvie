{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, MultiParamTypeClasses #-}

module Language.Forvie.Parsing.Parser where

import           Data.Char (isAlpha) -- only needed for the example
import           Control.Monad (unless)
import           Control.Monad.Trans (lift)
import           Control.Monad.State (StateT, gets, modify, execStateT)
import           Data.TypedMap (Equal (..), Compare1 (..), Ord1 (..), Eq1 (..), Show1 (..), P (..))
import qualified Data.TypedMap as TM
import qualified Data.TypedSet as TS

-- TODO:
-- - better data structures for storing (pos,nt)->a maps
-- - tail calls
-- - kleene star? by translation into tail calls
-- - return values from non-terminals
-- - error reporting, with (graph) stack traces
-- - error recovery?
-- - integration with MonadicStream stuff
-- - generation of SPPFs, and resurrection of the graph drawing stuff: complete debugging suite
-- - derivation of Eq1, Show1 and Ord1 for non-terminals (is this possible with template Haskell yet?)
-- - doing it in a dependently typed language... with correctness proof
-- - stratified results; which would probably make the storage of waiting sets easier (maybe)


-- - gc-able representation of previous sets
--   - at the moment, we hang on to a reference (i,nt) even if no one will ever respond to it

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

instance Show1 v => Show (AST v a) where
    show (Atom c)    = "(Atom " ++ [c] ++ ")"
    show (Paren e)   = "(Paren " ++ show1 e ++ ")"
    show (Emp)       = "Emp"
    show (Cons e es) = "(Cons "++show1 e++" "++show1 es++")"

class Show2 f where
    show2 :: Show1 v => f v a -> String

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

--------------------------------------------------------------------------------

-- Overall plan:
-- - collect some grammars to test with
--   - with and without tailcalls (or by translation from Kleene star)
-- - collect benchmarks of different implementation strategies (ambiguity detection and storage of waiting sets)
-- - try out a CPS version too.
-- - get the error reporting working, and try it out on some realistic grammars

-- If you have an LL grammar, then this algorithm should be quite
-- fast...? If you have an LR(1) grammar then it should also be quite
-- fast? If you use tail calls? Is there any relationship between the
-- item sets that are built and the sets of LR(0) items? Would dynamic
-- caching of results be useful?

{----------------------------------------}
class Monad m => ParseResultsMonad f m where
    type ResultNode f m :: * -> *
    newResult :: Int -> Int -> f (ResultNode f m) x -> m (ResultNode f m x)
    addResult :: ResultNode f m x -> f (ResultNode f m) x -> m ()

class ParseResultsMonad f m => ParseStackMonad nt tok f m where
    data StackNode nt tok f m :: * -> *
    newStackNode     :: Int -> nt a -> m (StackNode nt tok f m a)
    addToStackNode   :: WaitingItem nt tok f m a -> StackNode nt tok f m a -> m ()
    readStackNode    :: StackNode nt tok f m a -> m [WaitingItem nt tok f m a]
    getPositionAndNT :: StackNode nt tok f m a -> m (Int, nt a)

  --parseError       :: Maybe tok -> [exists a. (T.Text, StackNode nt tok f m a)] -> m a

-- Could keep this abstract when defining ParseMonads?
data WaitingItem nt tok f m a where
    WItem :: (ResultNode f m a -> RHS nt tok (ResultNode f m) (f (ResultNode f m) b))
          -> StackNode nt tok f m b
          -> WaitingItem nt tok f m a

data Item nt tok f m where
    Item :: RHS nt tok (ResultNode f m) (f (ResultNode f m) b)
         -> StackNode nt tok f m b
         -> Item nt tok f m

create :: ResultNode f m a -> WaitingItem nt tok f m a -> Item nt tok f m
create a (WItem k returnAddress) = Item (k a) returnAddress

{----------------------------------------}
parse :: (Ord1 nt, ParseStackMonad nt tok f m) =>
         Grammar nt tok f
      -> nt a
      -> [tok]
      -> m (Maybe (ResultNode f m a))
parse grammar nt input = do
  stackNode <- newStackNode 0 nt
  go [Item (grammar nt) stackNode] 0 input
    where
      go items j [] = do
        (_, complete) <- executeAll grammar Nothing j items
        return (TM.lookup (P 0 nt) complete)
      go items j (tok:toks) = do
        (newItems, _) <- executeAll grammar (Just tok) j items
        go newItems (j+1) toks

-- If newItems is empty (or only contains “Failure”), then we have
-- encountered a parse error. Have a look at the 'items' list to
-- derive a stack-trace of what went wrong. We can also tag “Failure”
-- with an error-message to describe the fault.

{----------------------------------------}
data ParserState nt tok f m
    = ParserState { known      :: TM.Map (P Int nt) (ResultNode f m)      -- ^ complete sub-parses ending at this point
                  , called     :: TS.Set nt                               -- ^ calls that have been made here, used to prevent non-productive loops
                  , waiting    :: TM.Map (P Int nt) (StackNode nt tok f m)-- ^ references to stack nodes generated in this step
                  , continuing :: [Item nt tok f m]                       -- ^ items that will continue to the next step
                  }

initState :: ParserState nt tok f m
initState =
    ParserState { known      = TM.empty
                , called     = TS.empty
                , waiting    = TM.empty
                , continuing = []
                }

{----------------------------------------}
type M nt tok f m a = StateT (ParserState nt tok f m) m a

checkKnown :: (Eq1 nt, Monad m) => Int -> nt a -> M nt tok f m (Maybe (ResultNode f m a))
checkKnown i nt = gets (TM.lookup (P i nt) . known)

addKnown :: Monad m => Int -> nt a -> ResultNode f m a -> M nt tok f m ()
addKnown i nt v = modify $ \s -> s { known = TM.insert (P i nt) v (known s) }

addContinuing :: Monad m => Item nt tok f m -> M nt tok f m ()
addContinuing item = modify $ \s -> s { continuing = item : continuing s }

addWaitingItem :: (Eq1 nt, ParseStackMonad nt tok f m) =>
                  Int
               -> nt a
               -> WaitingItem nt tok f m a
               -> M nt tok f m (StackNode nt tok f m a)
addWaitingItem j nt wfcall = do
  maybeStackNode <- gets (TM.lookup (P j nt) . waiting)
  case maybeStackNode of
    Nothing -> do
      stackNode <- lift $ newStackNode j nt
      lift $ addToStackNode wfcall stackNode
      modify $ \s -> s { waiting = TM.insert (P j nt) stackNode (waiting s) }
      return stackNode
    Just stackNode -> do
      lift $ addToStackNode wfcall stackNode
      return stackNode

recordCall :: (Ord1 nt, Monad m) => nt a -> M nt tok f m Bool
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
           -> [Item nt tok f m]    -- ^ Items to be processed
           -> m ([Item nt tok f m], TM.Map (P Int nt) (ResultNode f m)) -- ^ The new items, and completed parses to this point
executeAll grammar token j items = do
  st <- execStateT (mapM_ execute items) initState
  return (continuing st, known st)
    where
      execute (Item (Return a) stackNode) = do
        (i,nt) <- lift $ getPositionAndNT stackNode
        known  <- checkKnown i nt
        case known of
          Nothing -> do
            v <- lift $ newResult i j a
            addKnown i nt v
            mapM_ (execute . create v) =<< (lift $ readStackNode stackNode)
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
        stackNode <- addWaitingItem j nt (WItem k returnAddress)
        known <- checkKnown j nt
        case known of
          Just v  -> do
            execute (Item (k v) returnAddress)
          Nothing -> do
            alreadyCalled <- recordCall nt
            unless alreadyCalled $ execute (Item (grammar nt) stackNode)
{-
      execute (Item (TailCall nt) returnAddress) = do
        alreadyCalled <- recordTailCall nt returnAddress
        unless alreadyCalled $ execute (Item (grammar nt) returnAddress)
-}
