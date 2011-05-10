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
    Term  :: NT PrecLevel Term
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
grammar Decls =
      File <$> list (nt Decl <* terminal T.Semicolon)

grammar Decl =
      Assumption <$  terminal T.Assume <*> nt Iden <* terminal T.Colon <*> setLevel 10 (nt Term)
  <|> TypeDecl   <$> nt Iden <* terminal T.Colon <*> setLevel 10 (nt Term)
  <|> Definition <$> nt Iden <*> list (nt Iden) <* terminal T.Equals <*> setLevel 10 (nt Term)
  <|> Datatype   <$  terminal T.Data
                 <*> nt Iden
                 <*> list ((,) <$ terminal T.LParen <*> nt Iden <* terminal T.Colon <*> setLevel 10 (nt Term) <* terminal T.RParen)
                 <*  terminal T.Colon <* terminal T.Set <* terminal T.ColonEquals <*> list (nt Cons)

grammar Cons =
      Constr <$ terminal T.Pipe <*> nt Iden <* terminal T.Colon <*> list (setLevel 0 (nt Term))

grammar Iden =
      Identifier <$> terminal T.Ident

grammar Term =
      atLevel 0  (Var <$> nt' Iden)
  <|> atLevel 0  (Paren <$ terminal T.LParen <*> reset (nt Term) <* terminal T.RParen)
  <|> atLevel 10 (Lam <$  terminal T.Lambda <*> nonEmptyList (nt' Iden) <* terminal T.FullStop <*> nt Term)
  <|> atLevel 10 (Pi
                  <$  terminal T.LParen
                  <*> nonEmptyList (nt' Iden)
                  <*  terminal T.Colon
                  <*> nt Term
                  <*  terminal T.RParen
                  <*  terminal T.Arrow
                  <*> nt Term)
  <|> atLevel 10 (Sigma
                  <$  terminal T.LParen
                  <*> nonEmptyList (nt' Iden)
                  <*  terminal T.Colon
                  <*> nt Term
                  <*  terminal T.RParen
                  <*  terminal T.Times
                  <*> nt Term)
  <|> atLevel 10 (Arr <$> down (nt Term) <*  terminal T.Arrow <*> nt Term)
  <|> atLevel 9  (Sum <$> down (nt Term) <*  terminal T.Plus <*> nt Term)
  <|> atLevel 9  (Desc_Sum <$> down (nt Term) <* terminal T.QuotePlus <*> nt Term)
  <|> atLevel 8  (Prod <$> down (nt Term) <*  terminal T.Times <*> nt Term)
  <|> atLevel 8  (Desc_Prod <$> down (nt Term) <*  terminal T.QuoteTimes <*> nt Term)
  <|> atLevel 1  (Inl <$  terminal T.Inl <*> down (nt Term))
  <|> atLevel 1  (Inr <$  terminal T.Inr <*> down (nt Term))
  <|> atLevel 1  (Desc_K <$ terminal T.QuoteK <*> down (nt Term))
  <|> atLevel 1  (Mu     <$ terminal T.Mu <*> down (nt Term))
  <|> atLevel 1  (Construct <$ terminal T.Construct <*> down (nt Term))
  <|> atLevel 1  (IDesc_Id  <$ terminal T.Quote_IId <*> down (nt Term))
  <|> atLevel 1  (IDesc_Sg  <$ terminal T.Quote_Sg <*> down (nt Term) <*> down (nt Term))
  <|> atLevel 1  (IDesc_Pi  <$ terminal T.Quote_Pi <*> down (nt Term) <*> down (nt Term))
  <|> atLevel 1  (App <$> down (nt Term) <*> nonEmptyList (down (nt Term)))
 -- FIXME: should unary operators be a level 1 or level 0?
  <|> atLevel 0  (Proj1 <$ terminal T.Fst <*> nt Term)
  <|> atLevel 0  (Proj2 <$ terminal T.Snd <*> nt Term)
  <|> atLevel 0  (MuI   <$ terminal T.MuI <*> nt Term <*> nt Term)
  <|> atLevel 0  (Induction <$ terminal T.Induction)
  <|> atLevel 0  (Desc_Elim <$ terminal T.ElimD)
  <|> atLevel 0  (UnitI     <$ terminal T.UnitValue)
  <|> atLevel 0  (Pair  <$ terminal T.LDoubleAngle <*> reset (nt Term) <* terminal T.Comma <*> reset (nt Term) <* terminal T.RDoubleAngle)
  <|> atLevel 0  (Case
                  <$  terminal T.Case
                  <*> reset (nt Term)
                  <*  terminal T.For <*> nt' Iden <*  terminal T.FullStop <*> reset (nt Term) <*  terminal T.With
                  <*  terminal T.LBrace
                  <*  terminal T.Inl <*> nt' Iden <* terminal T.FullStop <*> reset (nt Term)
                  <*  terminal T.Semicolon
                  <*  terminal T.Inr <*> nt' Iden <* terminal T.FullStop <*> reset (nt Term)
                  <*  terminal T.RBrace)
  <|> atLevel 0  (Set <$ terminal T.Set <*> (pure 0 <|> (read . T.unpack <$> terminal T.Number)))
  <|> atLevel 0  (Empty <$ terminal T.EmptyType)
  <|> atLevel 0  (ElimEmpty <$ terminal T.ElimEmpty)
  <|> atLevel 0  (Unit <$ terminal T.UnitType)
  <|> atLevel 0  (Desc_Id <$ terminal T.QuoteId)
  <|> atLevel 0  (Desc <$ terminal T.Desc)
  <|> atLevel 0  (IDesc <$ terminal T.IDesc)
  <|> atLevel 0  (IDesc_Elim <$ terminal T.IDesc_Elim)
