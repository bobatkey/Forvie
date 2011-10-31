module Language.Forvie.Util.Templater where

import           Prelude hiding (foldl)
import           Control.Monad.Identity (runIdentity)
import           Data.Monoid (mempty, mappend)
import           Data.Char (ord)
import           Data.Word (Word8)
import qualified Data.Map as M
import qualified Data.RangeSet as RS
import qualified Data.DFA as DFA
import qualified Data.ByteString as B
import           Data.MonadicStream
import           Language.Forvie.Lexing.Spec
import           Language.Forvie.Lexing.ByteString

data Token = Text | Variable deriving (Show, Eq, Ord)

dollar :: Word8
dollar = fromInteger $ toInteger $ ord '$'

dollarS :: RS.Set Word8
dollarS = singleton dollar

alphaNum :: RS.Set Word8
alphaNum = interval cA cZ .|. interval ca cz .|. interval c0 c9
    where cA = fromInteger $ toInteger $ ord 'A'
          cZ = fromInteger $ toInteger $ ord 'Z'
          ca = fromInteger $ toInteger $ ord 'a'
          cz = fromInteger $ toInteger $ ord 'z'
          c0 = fromInteger $ toInteger $ ord '0'
          c9 = fromInteger $ toInteger $ ord '9'

dfa :: DFA.DFA Word8 Token
dfa = DFA.makeDFA
      [ tok dollarS .>>. oneOrMore (tok alphaNum) .>>. tok dollarS :==> Variable
      , oneOrMore (tok (complement dollarS))                       :==> Text
      , tok dollarS                                                :==> Text
      ]

dropDollars :: B.ByteString -> B.ByteString
dropDollars =
    B.takeWhile (/=dollar) . B.dropWhile (==dollar)

-- FIXME: more customisable error correction in lexers
-- FIXME: implement composition of streams with processors
applyVariableSubstitution :: [(B.ByteString, B.ByteString)]
                          -> B.ByteString
                          -> B.ByteString
applyVariableSubstitution substitution input =
    runIdentity (lexer dfa (OnError $ return Text) input |>| foldl processChunk mempty)
    where
      substMap = M.fromList substitution

      processChunk d (Variable, s) =
          let s' = dropDollars s in
          case M.lookup s' substMap of
            Nothing -> d `mappend` s'
            Just t  -> d `mappend` t
      processChunk d (Text,     s) =
          d `mappend` s

{-
alphaNum :: CharSet
alphaNum = interval 'A' 'Z' .|. interval 'a' 'z' .|. interval '0' '9'

lexicalSpec :: CompiledLexSpec Token
lexicalSpec = compileLexicalSpecification
    [ "$" .>>. oneOrMore (tok alphaNum) .>>. "$"   :==> Variable
    , oneOrMore (tok (complement (singleton '$'))) :==> Text
    , "$"                                          :==> Text
    ]

applySubstToLine :: M.Map Text Text
           -> Text
           -> Text
applySubstToLine substitution text =
    lexer lexicalSpec text |>| 


applyVariableSubstitution :: [(Text,Text)] -> Processor Text m Text
applyVariableSubstitution substitution =
    mapM (applySubst $ M.fromList substitution)

findVariables :: LexingError e => SP e Char (Lexeme Token)
findVariables = lexerSP lexicalSpec

stripName :: Text -> Text
stripName = dropAround (=='$')

doSubst :: M.Map Text Text
        -> Lexeme Token
        -> Text
doSubst s (Lexeme Text     _ txt) = txt
doSubst s (Lexeme Variable _ varname) =
    case M.lookup (stripName varname) s of
      Nothing -> varname
      Just t  -> t

applyVariableSubstitution :: (UTF8DecodeError e, LexingError e) =>
                             [(Text, Text)]
                          -> SP e ByteString ByteString
applyVariableSubstitution subst =
    toWord8 >>>
    decodeUTF8 >>>
    findVariables >>>
    mapSP (doSubst $ M.fromList subst) >>>
    mapSP encodeUtf8
-}