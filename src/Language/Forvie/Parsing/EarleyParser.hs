{-# LANGUAGE KindSignatures, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeOperators #-}

module Language.Forvie.Parsing.EarleyParser where

import Control.Applicative
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Data.Text (Text)
import Data.Type.Eq
import Data.Type.Equality
import Data.Type.Show
import Control.Monad.State

import Language.Forvie.Parsing.Grammar
import Text.Lexeme

--------------------------------------------------------------------------------
class (Functor m, Monad m) => ParseResultsMonad f v m where
    newResult :: Int -> Int -> f v x -> m (v x)
    addResult :: v x -> f v x -> m ()

class (Functor m, Monad m) => ParseSourceMonad tok m where
    getInput :: m (Maybe (Lexeme tok)) -- FIXME: write this in CPS

--------------------------------------------------------------------------------
data Item nt tok (f :: (* -> *) -> * -> *) v t where
    Item :: RHS nt tok v (f v b)
         -> RetAddr nt tok f v t b
         -> Item nt tok f v t

data PreItem nt tok (f :: (* -> *) -> * -> *) v t a where
    PreItem :: (v a -> RHS nt tok v (f v b))
            -> RetAddr nt tok f v t b
            -> PreItem nt tok f v t a
               

data RetAddr nt tok f v t b where
    Local    :: Call nt b -> RetAddr nt tok f v t b
    TopLevel :: RetAddr nt tok f v t t
    Previous :: Int -> Call nt b -> [PreItem nt tok f v t b] -> RetAddr nt tok f v t b

mkItem :: v b -> PreItem nt tok f v t b -> Item nt tok f v t
mkItem v (PreItem rhs ra) = Item (rhs v) ra

instance Show3 nt => Show2 (RetAddr nt tok f v t) where
    show2 (Previous i call _) = "(Previous " ++ show i ++ " " ++ show2 call ++ ")"
    show2 TopLevel    = "TopLevel"
    show2 (Local call) = "(Local " ++ show2 call ++ ")"

--------------------------------------------------------------------------------
parse :: (Eq3 nt,
          Show3 nt,
          Eq  tok,
          Ord  a,
          Show a,
          ParseResultsMonad f v m,
          ParseSourceMonad tok m) =>
         Grammar f nt tok
      -> nt a t
      -> a
      -> m [f v t]
parse grammar r a = expander 0 [Item rhs TopLevel]
    where
      rhs = getRHS grammar (Call r a 0)

      expander i ps = do
        t <- getInput
        (wft, c) <- expand grammar i ps t
        case t of
          Nothing -> return c
          Just _  -> expander (i+1) wft

parseList :: (Eq3 nt,
              Show3 nt,
              Eq  tok,
              Show tok,
              Ord  a,
              Show a,
              ParseResultsMonad f v m,
              Ord tok) =>
             Grammar f nt tok
          -> nt a t
          -> a
          -> [Lexeme tok]
          -> m [f v t]
parseList grammar r a = expander 0 [Item rhs TopLevel]
    where
      rhs = getRHS grammar (Call r a 0)

      expander i ps input = do
        let (t,input') = case input of [] -> (Nothing,[]); (t:ts) -> (Just t,ts)
        (wft, c) <- expand grammar i ps t
        case t of
          Nothing -> return c
          Just _  -> expander (i+1) wft input'

--------------------------------------------------------------------------------
doItem :: Eq3 nt => Int -> [WaitingForCall nt tok f v t] -> Item nt tok f v t -> Item nt tok f v t
doItem j mapping (Item rhs (Local call)) = Item rhs (Previous j call (findCalls call mapping))
doItem j mapping (Item rhs ra)           = Item rhs ra

data WaitingForCall nt tok f v t where
    WfCallRA :: Call nt a ->
                (v a -> RHS nt tok v (f v b)) ->
                RetAddr nt tok f v t b ->
                WaitingForCall nt tok f v t

relevantCall :: Eq3 nt => Call nt a -> Call nt b -> Maybe (a :=: b)
relevantCall (Call nt1 a1 l1) (Call nt2 a2 l2) =
    case nt1 ==== nt2 of
      Just (Refl,Refl) -> if a1 == a2 && l1 <= l2 then Just Refl else Nothing
      Nothing          -> Nothing

findCalls :: Eq3 nt => Call nt a -> [WaitingForCall nt tok f v t] -> [PreItem nt tok f v t a]
findCalls call [] = []
findCalls call (WfCallRA call' rhs ra : wfcalls) =
    case call `relevantCall` call' of
      Just Refl -> PreItem rhs ra : findCalls call wfcalls
      Nothing   -> findCalls call wfcalls

processCalls :: Eq3 nt => Int -> [WaitingForCall nt tok f v t] -> [WaitingForCall nt tok f v t] -> [WaitingForCall nt tok f v t]
processCalls j mapping [] = []
processCalls j mapping (WfCallRA call rhs (Local call') : wfcalls)
    = WfCallRA call rhs (Previous j call' (findCalls call' mapping)) : processCalls j mapping wfcalls
processCalls j mapping (WfCallRA call rhs ra : wfcalls)
    = WfCallRA call rhs ra : processCalls j mapping wfcalls

data SomeCall nt where
    SomeCall :: Call nt a -> SomeCall nt

instance Eq3 nt => Eq (SomeCall nt) where
    SomeCall c == SomeCall c' = case c === c' of
                                  Nothing -> False
                                  Just _  -> True

--------------------------------------------------------------------------------
data Result v nt where
    Result :: Call nt a -> v a -> Result v nt

--------------------------------------------------------------------------------
data St nt tok f v t
    = St { parsed          :: IM.IntMap [Result v nt]       -- start point, nonterminal recognised
         , called          :: [SomeCall nt]                 -- non-terminals we have predicted at this point
         , waitingForCall  :: [WaitingForCall nt tok f v t] -- processes that are waiting for the completion of a non-terminal
         , newItems        :: [Item nt tok f v t] -- processes that are waiting for a token
         , completeParse   :: [f v t]                       -- whether a complete parse of the input has been discovered here
         }

initState :: St nt tok f v t
initState = St { parsed          = IM.empty
               , called          = []
               , waitingForCall  = []
               , newItems        = []
               , completeParse   = []
               }

type M nt tok f v t m a =
    StateT (St nt tok f v t) m a

-- FIXME: should this really check for relevant calls? I think so.
checkKnown :: forall nt tok f v b t m.
              (Eq3 nt, Monad m, Functor m) =>
              Int
           -> Call nt b
           -> M nt tok f v t m (Maybe (v b))
checkKnown i call = join . fmap findCall . IM.lookup i <$> gets parsed
    where
      findCall :: [Result v nt] -> Maybe (v b)
      findCall []                         = Nothing
      findCall (Result call' a : results) = do Refl <- call === call'; return a
                                            <|>
                                            findCall results

addKnown :: Monad m => 
            Int
         -> Call nt a
         -> v a
         -> M nt tok f v t m ()
addKnown i call b =
    modify $ \s -> s { parsed = IM.alter (Just . (Result call b:) . fromMaybe []) i (parsed s) }

addItem :: Monad m =>
           RHS nt tok v (f v b)
        -> RetAddr nt tok f v t b
        -> M nt tok f v t m ()
addItem p retAddr =
    modify $ \s -> s { newItems = Item p retAddr : newItems s }

recordCalled :: (Eq3 nt, Monad m) =>
                Call nt a
             -> M nt tok f v t m Bool
recordCalled call = do
  p <- gets called >>= return . elem (SomeCall call)
  unless p $ modify $ \s -> s { called = (SomeCall call) : (called s) }
  return p

addWaitingForCall :: Monad m => 
                     Call nt a ->
                     (v a -> RHS nt tok v (f v b)) ->
                     RetAddr nt tok f v t b ->
                     M nt tok f v t m ()
addWaitingForCall call cont retAddr = do
  modify $ \s -> s { waitingForCall = WfCallRA call cont retAddr : waitingForCall s }

-- FIXME: Need to ignore the waiting processes in the current item set
-- that arise from inside stars. See the changes made to EarleyRecogniser.lhs
-- Port the generation of items back to EarleyRecogniser.lhs

addComplete :: Monad m => f v t -> M nt tok f v t m ()
addComplete t = modify $ \s -> s { completeParse = t : completeParse s }

--------------------------------------------------------------------------------
expand :: forall nt tok f v t m.
          (Eq tok, Eq3 nt, Show3 nt, ParseResultsMonad f v m) =>
          Grammar f nt tok
       -> Int
       -> [Item nt tok f v t]
       -> Maybe (Lexeme tok)
       -> m ( [Item nt tok f v t]
            , [f v t])
expand grammar j worklist inp = do
  St _ _ wfc wft complete <- execStateT (mapM_ process worklist) initState
  let mapping = processCalls j mapping wfc
      wft'    = map (doItem j mapping) wft
  return (wft', complete)
    where
      process :: Item nt tok f v t -> M nt tok f v t m ()
      process (Item p retAddr) = do
        mapM_ (processC retAddr) (components p)

      processC :: forall b.
                  RetAddr nt tok f v t b
               -> Component nt tok v (f v b)
               -> M nt tok f v t m ()
      processC retAddr (WfToken cs p) =
          case inp of
            Just (Lexeme t _ x) | cs == t -> addItem (p x) retAddr
            _                             -> return ()

      processC TopLevel    (Accept t) = do
        addComplete t

      processC (Local call) (Accept t) = do
        known <- checkKnown j call
        case known of
          Nothing -> do v <- lift $ newResult j j t
                        addKnown j call v
                        completed <- map (mkItem v) . findCalls call <$> gets waitingForCall
                        mapM_ process completed
          Just v  -> do lift $ addResult v t

      processC (Previous i call l) (Accept t) = do
        known <- checkKnown i call
        case known of
          Nothing -> do v <- lift $ newResult i j t
                        addKnown i call v
                        mapM_ process $ map (mkItem v) l
          Just v  -> do lift $ addResult v t

      processC retAddr (WfCall insideStar call p) = do
        addWaitingForCall call p retAddr
        let loop call@(Call nt x l) =
                do called <- recordCalled call
                   if called then return ()
                    else do process (Item (grammar `getRHS` call) (Local call))
                            completion <-
                               if not insideStar then
                                   do known <- checkKnown j call
                                      case known of
                                        Nothing -> return ()
                                        Just v  -> process (Item (p v) retAddr)
                               else
                                   return ()
                            if l == 0 then return () else loop (Call nt x (l-1))
        loop call
