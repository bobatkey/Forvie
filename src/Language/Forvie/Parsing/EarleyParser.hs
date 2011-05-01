{-# LANGUAGE KindSignatures, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}

module Language.Forvie.Parsing.EarleyParser where

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Type.Eq
import Data.Type.Equality
import Data.Type.Show
import Control.Monad.State
import Control.Monad.Reader

--import Debug.Trace

import Language.Forvie.Parsing.Grammar
import Text.Lexeme

--------------------------------------------------------------------------------
class (Functor m, Monad m) => ParseResultsMonad f v m where
    newResult :: Int -> Int -> f v x -> m (v x)
    addResult :: v x -> f v x -> m ()

class (Functor m, Monad m) => ParseSourceMonad tok m where
    getInput :: m (Maybe (Lexeme tok)) -- FIXME: write this in CPS

--------------------------------------------------------------------------------
data Item rhs nt tok (f :: (* -> *) -> * -> *) v t where
    Item :: rhs nt tok v () (f v b)
         -> RetAddr nt t b
         -> Item rhs nt tok f v t

data RetAddr nt t b where
    NT       :: Int -> Call nt b -> RetAddr nt t b
    TopLevel :: RetAddr nt t t

instance Show3 nt => Show2 (RetAddr nt t) where
    show2 (NT i call) = "(NT " ++ show i ++ " " ++ show2 call ++ ")"
    show2 TopLevel    = "TopLevel"

--------------------------------------------------------------------------------
parse :: (Eq3 nt,
          Eq  tok,
          Eq  a,
          Show a,
          RHS rhs,
          ParseResultsMonad f v m,
          ParseSourceMonad tok m) =>
         Grammar rhs f nt tok
      -> nt a t
      -> a
      -> m [f v t]
parse grammar r a = expander M.empty 0 [Item rhs TopLevel]
    where
      rhs = getRHS grammar (Call r a)

      expander previous i ps = do
        (wfc, wft, c) <- expand grammar previous i ps
        t <- getInput
        consumer (M.insert i wfc previous) (i+1) wft c t

      consumer previous i wft c Nothing  = return c
      consumer previous i wft _ (Just t) =
          expander previous i (advance t wft)

--------------------------------------------------------------------------------
-- 't' for toplevel (and 'v' for variable)
data WaitingForToken rhs nt tok f v t where
    WfTokenRA :: tok ->
                 rhs nt tok v Text (f v b) ->
                 RetAddr nt t b ->
                 WaitingForToken rhs nt tok f v t

data WaitingForCall rhs nt tok f v t where
    WfCallRA :: Call nt a ->
                rhs nt tok v (v a) (f v b) ->
                RetAddr nt t b ->
                WaitingForCall rhs nt tok f v t

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
data St rhs nt tok f v t
    = St { parsed          :: M.Map Int [Result v nt]       -- start point, nonterminal recognised
         , called          :: [SomeCall nt]                 -- non-terminals we have predicted at this point
         , waitingForCall  :: [WaitingForCall rhs nt tok f v t] -- processes that are waiting for the completion of a non-terminal
         , waitingForToken :: [WaitingForToken rhs nt tok f v t] -- processes that are waiting for a token
         , completeParse   :: [f v t]                       -- whether a complete parse of the input has been discovered here
         }

initState :: St rhs nt tok f v t
initState = St { parsed          = M.empty
               , called          = []
               , waitingForCall  = []
               , waitingForToken = []
               , completeParse   = []
               }

type Previous rhs nt tok f v t = M.Map Int [WaitingForCall rhs nt tok f v t]

type M rhs nt tok f v t m a =
    ReaderT (Previous rhs nt tok f v t)
            (StateT (St rhs nt tok f v t) m)
            a

checkKnown :: forall rhs nt tok f v b t m.
              (Eq3 nt, Monad m, Functor m) =>
              Int
           -> Call nt b
           -> M rhs nt tok f v t m (Maybe (v b))
checkKnown i call = join . fmap findCall . M.lookup i <$> gets parsed
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
         -> M rhs nt tok f v t m ()
addKnown i call b =
    modify $ \s -> s { parsed = M.alter (Just . (Result call b:) . fromMaybe []) i (parsed s) }

addWaitingForChar :: Monad m =>
                     tok
                  -> rhs nt tok v Text (f v b)
                  -> RetAddr nt t b
                  -> M rhs nt tok f v t m ()
addWaitingForChar cs p retAddr =
    modify $ \s -> s { waitingForToken = WfTokenRA cs p retAddr : waitingForToken s }

recordCalled :: (Eq3 nt, Monad m, Functor m) =>
                Call nt a
             -> M rhs nt tok f v t m Bool
recordCalled call = do
  p <- elem (SomeCall call) <$> gets called
  unless p $ modify $ \s -> s { called = SomeCall call : called s }
  return p

addWaitingForCall :: Monad m => 
                     Call nt a ->
                     rhs nt tok v (v a) (f v b) ->
                     RetAddr nt t b ->
                     M rhs nt tok f v t m ()
addWaitingForCall call cont retAddr = do
  modify $ \s -> s { waitingForCall = WfCallRA call cont retAddr : waitingForCall s }

-- FIXME: Need to ignore the waiting processes in the current item set
-- that arise from inside stars. See the changes made to EarleyRecogniser.lhs
-- Port the generation of items back to EarleyRecogniser.lhs
getCompletions :: forall rhs nt tok f v t a m.
                  (Eq3 nt, RHS rhs, Monad m, Functor m) =>
                  Int
               -> Int
               -> Call nt a
               -> v a
               -> M rhs nt tok f v t m [Item rhs nt tok f v t]
getCompletions i j call a
    | i == j    = mapMaybe extract <$> gets waitingForCall
    | otherwise = mapMaybe extract . fromMaybe [] . M.lookup i <$> ask
    where
      extract :: WaitingForCall rhs nt tok f v t -> Maybe (Item rhs nt tok f v t)
      extract (WfCallRA call' k ra) = do Refl <- call === call'
                                         return (Item (k $$ a) ra)

addComplete :: Monad m => f v t -> M rhs nt tok f v t m ()
addComplete t = modify $ \s -> s { completeParse = t : completeParse s }

--------------------------------------------------------------------------------
expand :: forall rhs nt tok f v t m.
          (RHS rhs, Eq3 nt, ParseResultsMonad f v m) =>
          Grammar rhs f nt tok
       -> Previous rhs nt tok f v t
       -> Int
       -> [Item rhs nt tok f v t]
       -> m ( [WaitingForCall rhs nt tok f v t]
            , [WaitingForToken rhs nt tok f v t]
            , [f v t])
expand grammar previous j worklist = do
  St _ _ wfc wft complete <-
     execStateT (runReaderT (process worklist)
                            previous)
                initState
  --trace ("(length wfc, length wft) = " ++ show (length wfc, length wft)) $ return ()
  return (wfc, wft, complete)
    where
      process :: [Item rhs nt tok f v t] -> M rhs nt tok f v t m ()
      process [] = return ()
      process (Item p retAddr : worklist) = do
        new <- concat <$> mapM (processC retAddr) (components p)
        process (new ++ worklist)

      processC :: RetAddr nt t b
               -> Component nt tok rhs v (f v b)
               -> M rhs nt tok f v t m [Item rhs nt tok f v t]
      processC retAddr (WfToken cs p) = do
        addWaitingForChar cs p retAddr
        return []

      processC TopLevel    (Accept t) = do
        addComplete t
        return []

      processC (NT i call) (Accept t) = do
        known <- checkKnown i call
        case known of
          Nothing -> do v <- lift $ lift (newResult i j t)
                        addKnown i call v
                        --trace ("Completing " ++ show (i,j)) $ return ()
                        getCompletions i j call v
          Just v  -> do lift $ lift (addResult v t)
                        return []

      processC retAddr (WfCall insideStar call p) = do
        addWaitingForCall call p retAddr
        called <- recordCalled call
        let calls = if called then []
                    else [Item (grammar `getRHS` call)
                               (NT j call)]
        completion <-
            if not insideStar then
                do known <- checkKnown j call
                   case known of
                     Nothing -> return []
                     Just v  -> return [Item (p $$ v)
                                             retAddr]
            else
                return []
        return (calls ++ completion)

--------------------------------------------------------------------------------
advance :: (Eq tok, RHS rhs) =>
           Lexeme tok
        -> [WaitingForToken rhs nt tok f v t]
        -> [Item rhs nt tok f v t]
advance i                  [] = []
advance i@(Lexeme t _ x) (WfTokenRA t' p retAddr : l)
    = if t == t' then
          Item (p $$ x) retAddr : advance i l
      else
          advance i l
