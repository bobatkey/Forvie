{-# LANGUAGE TypeFamilies #-}

-- FIXME: haddock header

module Data.FiniteStateMachine.ThompsonAbstractMachine
    ( Regexp (..)
    , compile

    , Machine
    , empty
    , epsilon
    , token
    , choice
    , (.>>.)
    , oneOrMore
    , zeroOrMore
    , execute )
    where

import           Data.FiniteStateMachine (FiniteStateMachine (..))
import qualified Data.RangeSet as RS
import           Data.Array (listArray, (!), Array, bounds, array)
import           Data.Monoid (mconcat)
import qualified Data.IntSet as IS

--------------------------------------------------------------------------------
data Regexp alphabet
    = Literal (RS.Set alphabet)
    | Alt     (Regexp alphabet) (Regexp alphabet)
    | Seq     (Regexp alphabet) (Regexp alphabet)
    | Star    (Regexp alphabet)
    | Emp
    | Eps

compile :: Regexp alphabet
        -> Machine alphabet
compile re =
    Machine (listArray (0,j) (instrs [Match]))
    where
      (j, instrs) = compileExp re 0

      compileExp (Literal c) i =
          (i+1, (Token c:))
      compileExp (Alt re1 re2) i =
          let (j, instrs1) = compileExp re1 (i+1)
              (k, instrs2) = compileExp re2 (j+1)
          in (k, (Split (i+1) (j+1):) . instrs1 . (Jump k:) . instrs2)
      compileExp (Seq re1 re2) i =
          let (j, instrs1) = compileExp re1 i
              (k, instrs2) = compileExp re2 j
          in (k, instrs1 . instrs2)
      compileExp (Star re) i =
          let (j, instrs) = compileExp re (i+1)
          in (j+1, (Split (i+1) (j+1):) . instrs . (Jump i:))
      compileExp Emp i =
          (i+1, (Reject:))
      compileExp Eps i =
          (i, id)

{-
testRE :: Regexp Char
testRE = Seq (Star (Literal (RS.singleton 'a')))
             (Star (Literal (RS.singleton 'b')))
-}

--------------------------------------------------------------------------------
data Instr alphabet
    = Token !(RS.Set alphabet)
    | Match
    | Jump  {-# UNPACK #-} !Int
    | Split {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    | Reject
    deriving (Eq, Show)

mapInstr :: (Int -> Int) -> Instr alphabet -> Instr alphabet
mapInstr f (Token s)       = Token s
mapInstr f Match           = Match
mapInstr f (Jump pc)       = Jump (f pc)
mapInstr f (Split pc1 pc2) = Split (f pc1) (f pc2)
mapInstr f Reject          = Reject

data Machine alphabet = Machine
    { mInstrs :: !(Array Int (Instr alphabet))
    } deriving Show

--------------------------------------------------------------------------------
instructionCount :: Machine alphabet -> Int
instructionCount (Machine instrs) =
    snd (bounds instrs) + 1

--------------------------------------------------------------------------------
empty :: Machine alphabet
empty = Machine (listArray (0,0) [Reject])

epsilon :: Machine alphabet
epsilon = Machine (listArray (0,0) [Match])

token :: RS.Set alphabet
      -> Machine alphabet
token s = Machine (listArray (0,1) [Token s, Match])

choice :: Machine alphabet
       -> Machine alphabet
       -> Machine alphabet
choice machine1 machine2 =
    Machine (array (0,newLength-1) instrList)
    where
      length1 = instructionCount machine1
      length2 = instructionCount machine2

      newLength = length1 + length2 + 1

      instrList = [ (i,instr i) | i <- [0..newLength-1] ]

      machine1offset = 1
      machine2offset = length1 + 1

      instr 0 = Split machine1offset machine2offset
      instr i
          | i <= length1 =
              mapInstr (+machine1offset)
                       (mInstrs machine1 ! (i-machine1offset))
          | otherwise    =
              mapInstr (+machine2offset)
                       (mInstrs machine2 ! (i-machine2offset))

(.>>.) :: Machine alphabet
       -> Machine alphabet
       -> Machine alphabet
machine1 .>>. machine2 =
    Machine (array (0,max_pc-1) instrList)
    where
      length1 = instructionCount machine1
      length2 = instructionCount machine2

      max_pc = length1 + length2

      instrList = [ (i,instr i) | i <- [0..max_pc-1] ]

      instr i
          | i < length1 =
              replaceMatch (Jump length1) (mInstrs machine1 ! i)
          | otherwise   =
              mapInstr (+length1) (mInstrs machine2 ! (i-length1))

oneOrMore :: Machine alphabet
          -> Machine alphabet
oneOrMore machine =
    Machine (array (0,newLength-1) instrList)
    where
      len = instructionCount machine

      newLength = len + 1

      instrList = [ (i, instr i) | i <- [0..newLength-1] ]

      instr i
          | i < len   =
              replaceMatch (Split 0 len) (mInstrs machine ! i)
          | otherwise =
              Match

replaceMatch :: Instr alphabet -> Instr alphabet -> Instr alphabet
replaceMatch x Match = x
replaceMatch _ i     = i

zeroOrMore :: Machine alphabet
           -> Machine alphabet
zeroOrMore machine = epsilon `choice` oneOrMore machine

--------------------------------------------------------------------------------
-- FIXME: presumably, a machine can be converted to a DFA by the usual
-- subset construction. Output results can be placed on the 'Match'
-- instructions.

-- 'save' instructions could be turned into 'per-thread' thing?
-- i.e. the DFA executor has a store of 'n'x'g' slots to store
-- pointers in? then the result says which slots to access, based on
-- which thread won?

close :: Machine alphabet
      -> Int
      -> IS.IntSet
close (Machine instrs) = closeState IS.empty 
    where
      closeState states pc =
          let states' = IS.insert pc states in
          case instrs ! pc of
            Token c  -> states'
            Match    -> states'
            Jump pc' ->
                closeState states' pc'
            Split pc1 pc2 ->
                closeState (closeState states' pc1) pc2
            Reject   -> states'

instance (Enum alphabet, Bounded alphabet, Ord alphabet) =>
    FiniteStateMachine (Machine alphabet) where
    type State    (Machine alphabet) = IS.IntSet
    type Alphabet (Machine alphabet) = alphabet
    type Result   (Machine alphabet) = ()

    -- the states of states are always closed
    initState machine =
        close machine 0

    -- assumption: @pcs@ is 'closed'
    advance machine c pcSet =
        IS.unions [ close machine q' |
                    q  <- IS.elems pcSet
                  , q' <- case mInstrs machine ! q of
                            Token s | RS.member c s -> [q+1]
                            _ -> []
                  ]

    isAcceptingState (Machine instrs) pcSet =
        mconcat [ Just () |
                  q <- IS.elems pcSet
                , instrs ! q == Match
                ]

    classes (Machine instrs) pcSet =
        mconcat [ RS.fromSet s |
                  q <- IS.elems pcSet
                , s <- case instrs ! q of
                         Token s -> [s]
                         _ -> []
                ]

--------------------------------------------------------------------------------
data Result
    = Continue IS.IntSet
    | Matched

-- For a more efficient imperative version:
-- - 'qConsidered' should be a bitmap of possible states
-- - 'qElements'   should be another bitmap of possible states, with a 'least' pointer?
-- - instructions could be encoded as integers, to get an efficient unboxed representation

data ThreadQueue = TQ !IS.IntSet ![Int]

(+:) :: Int -> ThreadQueue -> ThreadQueue
(+:) pc (TQ considered pcs)
    | IS.member pc considered =
        TQ considered pcs
    | otherwise =
        TQ (IS.insert pc considered) (pc:pcs)

infixr 5 +:

fromQueue :: ThreadQueue -> Maybe (Int, ThreadQueue)
fromQueue (TQ considered [])       = Nothing
fromQueue (TQ considered (pc:pcs)) = Just (pc, TQ considered pcs)

mkQueue :: IS.IntSet -> ThreadQueue
mkQueue threads = TQ threads (IS.elems threads)

execute :: Machine Char -> String -> Maybe String
execute machine input = go (IS.singleton 0) input
    where
      go threads [] =
          Nothing
      go threads (c:cs) =
          case processThreads c IS.empty (mkQueue threads) of
            Continue newThreads
                | IS.null newThreads ->
                    Nothing
                | otherwise ->
                    go newThreads cs
            Matched ->
                Just cs

      processThreads c newThreads queue =
          case fromQueue queue of
            Nothing ->
                Continue newThreads
            Just (pc, queue') ->
                case mInstrs machine ! pc of
                  Token s
                    | c `RS.member` s ->
                        processThreads c (IS.insert (pc+1) newThreads) queue'
                    | otherwise ->
                        processThreads c newThreads queue'
                  Match ->
                      Matched
                  Jump pc' ->
                      processThreads c newThreads (pc' +: queue') 
                  Split pc1 pc2 ->
                      processThreads c newThreads (pc1 +: pc2 +: queue')
                  Reject ->
                      processThreads c newThreads queue'