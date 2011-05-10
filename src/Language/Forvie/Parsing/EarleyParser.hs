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
import Control.Monad.Reader

import Debug.Trace

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
          Eq  a,
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
        (wft, c) <- expand grammar i ps
        t <- getInput
        consumer (i+1) wft c t

      consumer i wft c Nothing  = return c
      consumer i wft _ (Just t) =
          expander i (advance t wft)

parseList :: (Eq3 nt,
              Show3 nt,
              Eq  tok,
              Show tok,
              Eq  a,
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
        (wft, c) <- expand grammar i ps
        consumer (i+1) wft c input

      consumer i wft c []  = return c
      consumer i wft _ (t:ts) =
          {-trace ("\nAdvancing on " ++ show (lexemeText t)) $-} expander i (advance t wft) ts

--------------------------------------------------------------------------------
-- 't' for toplevel (and 'v' for variable)
data WaitingForToken nt tok f v t where
    WfTokenRA :: tok ->
                 (Text -> RHS nt tok v (f v b)) ->
                 RetAddr nt tok f v t b ->
                 WaitingForToken nt tok f v t

doWFT :: Eq3 nt => Int -> [WaitingForCall nt tok f v t] -> [WaitingForToken nt tok f v t] -> [WaitingForToken nt tok f v t]
doWFT j mapping [] = []
doWFT j mapping (WfTokenRA tok rhs (Local call) : wfts) = WfTokenRA tok rhs (Previous j call (findCalls call mapping)) : doWFT j mapping wfts
doWFT j mapping (WfTokenRA tok rhs ra : wfts)           = WfTokenRA tok rhs ra : doWFT j mapping wfts

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
         , waitingForToken :: [WaitingForToken nt tok f v t] -- processes that are waiting for a token
         , completeParse   :: [f v t]                       -- whether a complete parse of the input has been discovered here
         }

initState :: St nt tok f v t
initState = St { parsed          = IM.empty
               , called          = []
               , waitingForCall  = []
               , waitingForToken = []
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

addWaitingForChar :: Monad m =>
                     tok
                  -> (Text -> RHS nt tok v (f v b))
                  -> RetAddr nt tok f v t b
                  -> M nt tok f v t m ()
addWaitingForChar cs p retAddr =
    modify $ \s -> s { waitingForToken = WfTokenRA cs p retAddr : waitingForToken s }

recordCalled :: (Eq3 nt, Monad m, Functor m) =>
                Call nt a
             -> M nt tok f v t m Bool
recordCalled call = do
  p <- elem (SomeCall call) <$> gets called
  unless p $ modify $ \s -> s { called = SomeCall call : called s }
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
          (Eq3 nt, Show3 nt, ParseResultsMonad f v m) =>
          Grammar f nt tok
       -> Int
       -> [Item nt tok f v t]
       -> m ( [WaitingForToken nt tok f v t]
            , [f v t])
expand grammar j worklist = do
  St _ _ wfc wft complete <- execStateT (process worklist) initState
  let mapping = processCalls j mapping wfc
      wft'    = doWFT j mapping wft
  --trace ("advance with " ++ show (length wft') ++ "\n") $
  return (wft', complete)
    where
      process :: [Item nt tok f v t] -> M nt tok f v t m ()
      process [] = return ()
      process (Item p retAddr : worklist) = do
        new <- concat <$> mapM (processC retAddr) (components p)
        process (new ++ worklist)

      processC :: forall b.
                  RetAddr nt tok f v t b
               -> Component nt tok v (f v b)
               -> M nt tok f v t m [Item nt tok f v t]
      processC retAddr (WfToken cs p) = do
        addWaitingForChar cs p retAddr
        return []

      processC TopLevel    (Accept t) = do
        addComplete t
        return []

      processC (Local call) (Accept t) = do
        known <- checkKnown j call
        case known of
          Nothing -> do v <- lift $ newResult j j t
                        addKnown j call v
                        map (mkItem v) . findCalls call <$> gets waitingForCall
          Just v  -> do lift $ addResult v t
                        return []

      processC (Previous i call l) (Accept t) = do
        known <- checkKnown i call
        case known of
          Nothing -> do v <- lift $ newResult i j t
                        addKnown i call v
                        --trace ("Completing " ++ show (length l) ++ " for " ++ show2 call) $
                        return $ map (mkItem v) l
          Just v  -> do lift $ addResult v t
                        return []

      processC retAddr (WfCall insideStar call p) = do
        addWaitingForCall call p retAddr
        let loop call@(Call nt x l) newitems =
                do called <- recordCalled call
                   if called then return newitems
                    else do calls <- return [Item (grammar `getRHS` call) (Local call)]
                            completion <-
                               if not insideStar then
                                   do known <- checkKnown j call
                                      case known of
                                        Nothing -> return []
                                        Just v  -> return [Item (p v) retAddr]
                               else
                                   return []
                            let newitems' = calls ++ completion ++ newitems
                            if l == 0 then return newitems' else loop (Call nt x (l-1)) newitems'
        loop call []

--------------------------------------------------------------------------------
advance :: (Eq tok) =>
           Lexeme tok
        -> [WaitingForToken nt tok f v t]
        -> [Item nt tok f v t]
advance i                  [] = []
advance i@(Lexeme t _ x) (WfTokenRA t' p retAddr : l)
    = if t == t' then
          Item (p x) retAddr : advance i l
      else
          advance i l
