{-# LANGUAGE GADTs, Arrows #-}

module Grammar
    ( grammar, NT (Decls) )
    where

import qualified Data.Text as T

import Control.Arrow
import Control.Applicative

import Data.Type.Equality
import Data.Type.Eq
import Data.Type.Show

import Language.Forvie.Parsing.Grammar
import Language.Forvie.Parsing.Monad

import qualified Token as T
import Display

--------------------------------------------------------------------------------
data NT a b where
    Decls :: NT ()        File
    Decl  :: NT ()        Declaration
    Term  :: NT ()        Term
    Iden  :: NT ()        Ident
    Cons  :: NT ()        Constructor

instance Eq3 NT where
    Decls ==== Decls = Just (Refl, Refl)
    Decl  ==== Decl  = Just (Refl, Refl)
    Term  ==== Term  = Just (Refl, Refl)
    Iden  ==== Iden  = Just (Refl, Refl)
    Cons  ==== Cons  = Just (Refl, Refl)
    _     ==== _     = Nothing

instance Show3 NT where
    show3 Decls = "Decls"
    show3 Decl  = "Decl"
    show3 Term  = "Term"
    show3 Iden  = "Iden"
    show3 Cons  = "Cons"

--------------------------------------------------------------------------------
grammar :: Grammar P AST NT T.Token
grammar Decls = noPrec
                (File <$> list (nt Decl <* terminal T.Semicolon))

grammar Decl = noPrec
     (Assumption <$  terminal T.Assume <*> nt Iden <* terminal T.Colon <*> reset (nt' Term)
  <|> TypeDecl   <$> nt Iden <* terminal T.Colon <*> reset (nt' Term)
  <|> Definition <$> nt Iden <*> list (nt Iden) <* terminal T.Equals <*> reset (nt' Term)
  <|> Datatype   <$  terminal T.Data
                 <*> nt Iden
                 <*> list ((,) <$ terminal T.LParen <*> nt Iden <* terminal T.Colon <*> reset (nt' Term) <* terminal T.RParen)
                 <*  terminal T.Colon <* terminal T.Set <* terminal T.ColonEquals <*> list (nt Cons))

grammar Cons = noPrec
      (Constr <$ terminal T.Pipe <*> nt Iden <* terminal T.Colon <*> list (setLevel 0 (nt' Term)))

grammar Iden = noPrec
      (Identifier <$> terminal T.Ident)

-- Idea: when we have a call to 'Term (PL 9)', the predictor ought to
-- spark off calls to everything below that (assuming they haven't
-- been called already). Change atLevel to be strict.

-- Idea is to simulate the effect of having a fallthrough case in the
-- grammar, but without marking the return values. So:

-- - When calling 'Term (PL 9)' the expander:
--   - calls 'Term (PL 9)' as normal
--   - generates a call to 'Term (PL 8)'

--   - generates a special item that awaits the response from the call
--     to 'Term (PL 8)', upon completion of this item with a variable
--     of type 'v Term', the variable is passed straight up to the
--     caller of 'Term (PL 9)'

-- normally, when the call to 'Term (PL 8)' completes, it will return
-- back up to callers of 'Term (PL 8)'. Want it to return to callers
-- of any precedence level above '8' (up to ten). Could do this by
-- fiddling the 'findCalls' function.

-- When doing a completion, we should let 'Term (PL 4)' complete
-- something that requires 'Term (PL 5)'.

grammar Term =
      atLevel 0  (Var <$> ntU Iden)
  <|> atLevel 0  (Paren <$ terminal T.LParen <*> reset (nt' Term) <* terminal T.RParen)
  <|> atLevel 4  (Lam <$  terminal T.Lambda <*> nonEmptyList (ntU Iden) <* terminal T.FullStop <*> nt' Term)
  <|> atLevel 4  (Pi
                  <$  terminal T.LParen
                  <*> nonEmptyList (ntU Iden)
                  <*  terminal T.Colon
                  <*> nt' Term
                  <*  terminal T.RParen
                  <*  terminal T.Arrow
                  <*> nt' Term)
  <|> atLevel 4  (Sigma
                  <$  terminal T.LParen
                  <*> nonEmptyList (ntU Iden)
                  <*  terminal T.Colon
                  <*> nt' Term
                  <*  terminal T.RParen
                  <*  terminal T.Times
                  <*> nt' Term)
  <|> atLevel 4  (Arr <$> down (nt' Term) <*  terminal T.Arrow <*> nt' Term)
  <|> atLevel 3  (Sum <$> down (nt' Term) <*  terminal T.Plus <*> nt' Term)
  <|> atLevel 3  (Desc_Sum <$> down (nt' Term) <* terminal T.QuotePlus <*> nt' Term)
  <|> atLevel 2  (Prod <$> down (nt' Term) <*  terminal T.Times <*> nt' Term)
  <|> atLevel 2  (Desc_Prod <$> down (nt' Term) <*  terminal T.QuoteTimes <*> nt' Term)
  <|> atLevel 1  (Inl <$  terminal T.Inl <*> down (nt' Term))
  <|> atLevel 1  (Inr <$  terminal T.Inr <*> down (nt' Term))
  <|> atLevel 1  (Desc_K <$ terminal T.QuoteK <*> down (nt' Term))
  <|> atLevel 1  (Mu     <$ terminal T.Mu <*> down (nt' Term))
  <|> atLevel 1  (Construct <$ terminal T.Construct <*> down (nt' Term))
  <|> atLevel 1  (IDesc_Id  <$ terminal T.Quote_IId <*> down (nt' Term))
  <|> atLevel 1  (IDesc_Sg  <$ terminal T.Quote_Sg <*> down (nt' Term) <*> down (nt' Term))
  <|> atLevel 1  (IDesc_Pi  <$ terminal T.Quote_Pi <*> down (nt' Term) <*> down (nt' Term))
  <|> atLevel 1  (App <$> down (nt' Term) <*> nonEmptyList (down (nt' Term)))
 -- FIXME: should unary operators be a level 1 or level 0?
  <|> atLevel 0  (Proj1 <$ terminal T.Fst <*> nt' Term)
  <|> atLevel 0  (Proj2 <$ terminal T.Snd <*> nt' Term)
  <|> atLevel 0  (MuI   <$ terminal T.MuI <*> nt' Term <*> nt' Term)
  <|> atLevel 0  (Induction <$ terminal T.Induction)
  <|> atLevel 0  (Desc_Elim <$ terminal T.ElimD)
  <|> atLevel 0  (UnitI     <$ terminal T.UnitValue)
  <|> atLevel 0  (Pair  <$ terminal T.LDoubleAngle <*> reset (nt' Term) <* terminal T.Comma <*> reset (nt' Term) <* terminal T.RDoubleAngle)
  <|> atLevel 0  (Case
                  <$  terminal T.Case
                  <*> reset (nt' Term)
                  <*  terminal T.For <*> ntU Iden <*  terminal T.FullStop <*> reset (nt' Term) <*  terminal T.With
                  <*  terminal T.LBrace
                  <*  terminal T.Inl <*> ntU Iden <* terminal T.FullStop <*> reset (nt' Term)
                  <*  terminal T.Semicolon
                  <*  terminal T.Inr <*> ntU Iden <* terminal T.FullStop <*> reset (nt' Term)
                  <*  terminal T.RBrace)
  <|> atLevel 0  (Set <$ terminal T.Set <*> (pure 0 <|> (read . T.unpack <$> terminal T.Number)))
  <|> atLevel 0  (Empty <$ terminal T.EmptyType)
  <|> atLevel 0  (ElimEmpty <$ terminal T.ElimEmpty)
  <|> atLevel 0  (Unit <$ terminal T.UnitType)
  <|> atLevel 0  (Desc_Id <$ terminal T.QuoteId)
  <|> atLevel 0  (Desc <$ terminal T.Desc)
  <|> atLevel 0  (IDesc <$ terminal T.IDesc)
  <|> atLevel 0  (IDesc_Elim <$ terminal T.IDesc_Elim)
