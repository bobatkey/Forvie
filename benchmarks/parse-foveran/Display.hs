{-# LANGUAGE GADTs, EmptyDataDecls, RankNTypes, OverloadedStrings #-}

module Display where

import Data.Knot
import Text.PrettyPrint
import qualified Data.Text as T
import qualified Identifier as I
import Data.String

--------------------------------------------------------------------------------
instance IsString Doc where
    fromString = text

--------------------------------------------------------------------------------
-- The syntactic categories
data File
data Declaration
data Term
data Ident
data Constructor

--------------------------------------------------------------------------------
data AST v a where
    File       :: [v Declaration]                -> AST v File

    Assumption :: v Ident -> v Term              -> AST v Declaration
    TypeDecl   :: v Ident -> v Term              -> AST v Declaration
    Definition :: v Ident -> [v Ident] -> v Term -> AST v Declaration
    Datatype   :: v Ident -> [(v Ident, v Term)]
                          -> [v Constructor]     -> AST v Declaration

    Constr     :: v Ident -> [v Term]            -> AST v Constructor

    Identifier :: I.Ident                      -> AST v Ident

    Paren :: v Term                            -> AST v Term
    Var   :: v Ident                           -> AST v Term
    Lam   :: [v Ident] -> v Term               -> AST v Term
    App   :: v Term  -> [v Term]               -> AST v Term
    Set   :: Int                               -> AST v Term
    Pi    :: [v Ident] -> v Term -> v Term     -> AST v Term
    Arr   :: v Term  -> v Term                 -> AST v Term
    Sigma :: [v Ident] -> v Term -> v Term     -> AST v Term
    Prod  :: v Term  -> v Term                 -> AST v Term
    Pair  :: v Term  -> v Term                 -> AST v Term
    Proj1 :: v Term                            -> AST v Term
    Proj2 :: v Term                            -> AST v Term
    Sum   :: v Term  -> v Term                 -> AST v Term
    Inl   :: v Term                            -> AST v Term
    Inr   :: v Term                            -> AST v Term
    Case  :: v Term -> v Ident -> v Term
                    -> v Ident -> v Term
                    -> v Ident -> v Term       -> AST v Term
    Unit  ::                                      AST v Term
    UnitI ::                                      AST v Term
    Empty ::                                      AST v Term
    ElimEmpty ::                                  AST v Term
    Desc      ::                                  AST v Term
    Desc_K    :: v Term                        -> AST v Term
    Desc_Id   ::                                  AST v Term
    Desc_Prod :: v Term -> v Term              -> AST v Term
    Desc_Sum  :: v Term -> v Term              -> AST v Term
    Desc_Elim ::                                  AST v Term
    Mu        :: v Term                        -> AST v Term
    Construct :: v Term                        -> AST v Term
    Induction ::                                  AST v Term

    IDesc      ::                                 AST v Term
    IDesc_Id   :: v Term                       -> AST v Term
    IDesc_Sg   :: v Term -> v Term             -> AST v Term 
    IDesc_Pi   :: v Term -> v Term             -> AST v Term
    IDesc_Elim ::                                 AST v Term
    MuI        :: v Term -> v Term             -> AST v Term

--------------------------------------------------------------------------------
-- FIXME: move this elsewhere
class Pretty f where
    pretty :: (forall a. v a -> Doc) -> f v a -> Doc

instance Pretty AST where
    pretty pp (File decls) = vcat $ map (\d -> pp d <> semi) decls
                             
    pretty pp (Assumption nm ty)     = "assume" <+> pp nm <+> colon <+> pp ty
    pretty pp (TypeDecl nm ty)       = pp nm <+> colon <+> pp ty
    pretty pp (Definition nm nms tm) = pp nm <+> hsep (map pp nms) <+> "=" <+> pp tm
    pretty pp (Datatype nm ps cons)  = "data" <+> pp nm <+> hsep (map ppParam ps) <+> colon <+> "Set" <+> ":=" <+> vcat (map pp cons)
        where ppParam (nm,tm) = "(" <> pp nm <+> colon <+> pp tm <> ")"

    pretty pp (Constr nm tms)        = "|" <+> pp nm <+> colon <+> hsep (map pp tms)

    pretty pp (Identifier ident)     = text $ T.unpack ident

    pretty pp (Paren tm)             = parens (pp tm)
    pretty pp (Var nm)               = pp nm
    pretty pp (Lam nms tm)           = "λ" <> hsep (map pp nms) <> "." <+> pp tm
    pretty pp (App tm tms)           = pp tm <+> sep (map pp tms)
    pretty pp (Set 0)                = "Set"
    pretty pp (Set l)                = "Set" <+> int l
    pretty pp (Pi nms tmA tmB)       = "(" <> hsep (map pp nms) <+> colon <+> pp tmA <> ")" <+> "→" <+> pp tmB
    pretty pp (Arr tmA tmB)          = pp tmA <+> "→" <+> pp tmB
    pretty pp (Sigma nms tmA tmB)    = "(" <> hsep (map pp nms) <+> colon <+> pp tmA <> ")" <+> "×" <+> pp tmB
    pretty pp (Prod tmA tmB)         = pp tmA <+> "×" <+> pp tmB
    pretty pp (Pair tmA tmB)         = "«" <> pp tmA <> comma <+> pp tmB <> "»"
    pretty pp (Proj1 tm)             = "fst" <+> pp tm
    pretty pp (Proj2 tm)             = "snd" <+> pp tm
    pretty pp (Sum tmA tmB)          = pp tmA <+> "+" <+> pp tmB
    pretty pp (Inl tm)               = "inl" <+> pp tm
    pretty pp (Inr tm)               = "inr" <+> pp tm
    pretty pp (Case tm x tmP
                       y tmL
                       z tmR)        = ("case" <+> pp tm <+> "for" <+> pp x <> "." <+> pp tmP <+> "with")
                                       $$
                                       nest 2 (("{" <+> hang ("inl" <+> pp y <> ".") 3 (pp tmL))
                                               $$
                                               (";" <+> hang ("inr" <+> pp z <> ".") 3 (pp tmR))
                                               $$
                                               "}")
    pretty pp UnitI                  = "⋄"
    pretty pp Unit                   = "Unit"
    pretty pp Empty                  = "Empty"
    pretty pp ElimEmpty              = "elimEmpty"
    pretty pp Desc                   = "Desc"
    pretty pp (Desc_K tm)            = "“K”" <+> pp tm
    pretty pp Desc_Id                = "“Id”"
    pretty pp (Desc_Prod tmA tmB)    = pp tmA <+> "“×”" <+> pp tmB
    pretty pp (Desc_Sum tmA tmB)     = pp tmA <+> "“+”" <+> pp tmB
    pretty pp Desc_Elim              = "elimD"
    pretty pp (Mu tm)                = "µ" <+> pp tm
    pretty pp (Construct tm)         = "construct" <+> pp tm
    pretty pp Induction              = "induction"
    pretty pp IDesc                  = "IDesc"
    pretty pp (IDesc_Id tm)          = "“IId”" <+> pp tm
    pretty pp (IDesc_Sg tmA tmB)     = "“Σ”" <+> pp tmA <+> pp tmB
    pretty pp (IDesc_Pi tmA tmB)     = "“Π”" <+> pp tmA <+> pp tmB
    pretty pp IDesc_Elim             = "elimID"
    pretty pp (MuI tmA tmB)          = "µI" <+> pp tmA <+> pp tmB

prettyKnot :: Pretty f => Knot f a -> Doc
prettyKnot (Knot x) = pretty prettyKnot x

--------------------------------------------------------------------------------
-- TODO:
-- - pretty printing for this form of syntax: don't need to worry about precedence
-- - conversion to locally nameless by resolving all the binding, and removing the parentheses
-- - think about using a separate identifier category, so that positions can be accurately captured
-- - data type declarations

-- binding information output:
-- for each occurence of an identifier in the output:
-- - either it is a binder, and should contain pointers to all its uses
-- - or it is a use, and should contain a pointer to its binder
-- - or it is a free variable, and we should do something but probably won't

