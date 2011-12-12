{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, MultiParamTypeClasses #-}

module Language.Forvie.Parsing.Parser
    ( parse
    , ParseResultsMonad (..)
    , ParseStackMonad (..)
    , WaitingItem
    )
    where

import           Control.Monad (unless)
import           Control.Monad.State (StateT, gets, modify, execStateT, lift)
import           Data.TypedMap (Ord1 (..), Eq1 (..), P (..))
import qualified Data.TypedMap as TM
import qualified Data.TypedSet as TS

import           Language.Forvie.Grammar.ActiveRHS

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
-- - stratified results; which makes ambiguity resolution easier


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
-- | Instances of this class are able to construct parse result trees,
-- and do on-the-fly ambiguity resolution.
class Monad m => ParseResultsMonad f m where
    type ResultNode f m :: * -> *
    newResult :: Int -> Int -> f (ResultNode f m) x -> m (ResultNode f m x)
    addResult :: ResultNode f m x -> f (ResultNode f m) x -> m ()

-- | Instances of this class are able to construct the stacks required
-- for keeping track of the nesting structure of while parsing. The
-- stacks may not necessarily be linear, and may be graph structured,
-- hence the need for a reference-like structure to hold them.
--
-- It would be nice to have a thingy that printed out the graphs for
-- each stage in the parsing process.
class ParseResultsMonad f m => ParseStackMonad nt tok f m where
    data StackNode nt tok f m :: * -> *
    newStackNode     :: Int -> nt a -> m (StackNode nt tok f m a)
    addToStackNode   :: WaitingItem nt tok f m a -> StackNode nt tok f m a -> m ()
    readStackNode    :: StackNode nt tok f m a -> m [WaitingItem nt tok f m a]
    getPositionAndNT :: StackNode nt tok f m a -> m (Int, nt a)

-- class ParseStackMonad nt tok f m => ParseErrorMonad nt tok f m where
--     parseError       :: Maybe tok -> [FailedItem nt tok f m] -> m a

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
-- towards a way of handling errors during parsing
{-
data FailedItem nt tok f m where
    FailedItem :: [String] -> StackNode nt tok f m a -> FailedItem nt tok f m

gatherFailure :: RHS nt tok v a -> Maybe [String]
gatherFailure (Failure message)  = Just [message]
gatherFailure (Choice rhs1 rhs2) = (++) <$> gatherFailure rhs1 <*> gatherFailure rhs2

gatherFailures :: [Item nt tok f m] -> Maybe [FailedItem nt tok f m]
gatherFailures [] = Just []
gatherFailures (Item rhs returnAddress:items) =
    (:) <$> (FailedItem <$> gatherFailure rhs <*> pure returnAddress) <*> gatherFailures items
-}

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
        -- if newItems is nothing but fail, then report an error or,
        -- plan B: make executeAll return all the Failures found
        -- either way, we seem to miss some??? Problem is that failed
        -- tokens will not be spotted until the next token is read
        -- could just do both...
        go newItems (j+1) toks

-- If newItems is empty (or only contains “Failure”), then we have
-- encountered a parse error. Have a look at the 'items' list to
-- derive a stack-trace of what went wrong. We can also tag “Failure”
-- with an error-message to describe the fault.

-- Plan: gather all the failures after each step
--       does this do the right thing? best to experiment
--       what if we get to the end and there is no visible failure, but no complete parse: report all the parse things still waiting?

-- could provide a result recovery strategy?

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

      execute (Item (Failure _) returnAddress) = do
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
