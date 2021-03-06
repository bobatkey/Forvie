{-# LANGUAGE GADTs, Arrows #-}

module Grammar
--    ( grammar, NT (Decls) )
    where

import qualified Data.Text as T

import Control.Applicative

import Data.Type.Equality
import Data.Type.Eq
import Data.Type.Show

import Language.Forvie.Parsing.Grammar

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
decls :: [v Declaration] -> RHS NT T.Token v (AST v File)
decls d = RHS [ Accept (File (reverse d))
              , WfCall False (Call Decl () 0) $ \v ->
                  RHS [ WfToken T.Semicolon $ \_ -> decls (v:d) ]
              ]

grammar :: Grammar AST NT T.Token
grammar Decls = \_ _ -> decls []
--                (File <$> list (call Decl <* terminal T.Semicolon))

grammar Decl = \_ _ ->
    RHS [ assume, typedeclOrDef, datatype ]
    where
      assume = WfToken T.Assume $ \_ ->
               RHS [ WfCall False (Call Iden () 0) $ \i ->
                     RHS [ WfToken T.Colon $ \_ ->
                           RHS [ WfCall False (Call Term () 4) $ \t ->
                                 RHS [ Accept (Assumption i t) ] ] ] ]

      typedeclOrDef = WfCall False (Call Iden () 0) $ \i ->
                      RHS [ WfToken T.Colon $ \_ ->
                            RHS [ WfCall False (Call Term () 4) $ \t ->
                                  RHS [ Accept (TypeDecl i t) ] ]
                          , WfToken T.Equals $ \_ ->
                            RHS [ WfCall False (Call Term () 4) $ \t ->
                                  RHS [ Accept (Definition i [] t) ] ]
                          , WfCall False (Call Iden () 0) $ \a -> def i [a]
                          ]

      def i a = RHS [ WfToken T.Equals $ \_ ->
                      RHS [ WfCall False (Call Term () 4) $ \t ->
                            RHS [ Accept (Definition i [] t) ] ]
                    , WfCall False (Call Iden () 0) $ \b -> def i (b:a)
                    ]

      datatype = WfToken T.Data $ \_ ->
                 Datatype <$> call Iden
                          <*> list ((,) <$ terminal T.LParen <*> call Iden <* terminal T.Colon <*> (callTop Term) <* terminal T.RParen)
                          <*  terminal T.Colon <* terminal T.Set <* terminal T.ColonEquals <*> list (call Cons)
                           


-- noPrec
--      (Assumption <$  terminal T.Assume <*> call Iden <* terminal T.Colon <*> (callTop Term)
--   <|> TypeDecl   <$> call Iden <* terminal T.Colon <*> (callTop Term)
--   <|> Definition <$> call Iden <*> list (call Iden) <* terminal T.Equals <*> (callTop Term)
--   <|> Datatype   <$  terminal T.Data
--                  <*> call Iden
--                  <*> list ((,) <$ terminal T.LParen <*> call Iden <* terminal T.Colon <*> (callTop Term) <* terminal T.RParen)
--                  <*  terminal T.Colon <* terminal T.Set <* terminal T.ColonEquals <*> list (call Cons))

grammar Cons = noPrec
      (Constr <$ terminal T.Pipe <*> call Iden <* terminal T.Colon <*> list (callAt Term 0))

grammar Iden = \_ _ -> RHS [ WfToken T.Ident $ \t -> RHS [ Accept (Identifier t) ] ]

-- noPrec
--       (Identifier <$> terminal T.Ident)

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
    \l () ->
        case l of
          4 -> term4
          3 -> term3
          2 -> term2
          1 -> term1
          0 -> term0

term4 :: RHS NT T.Token v (AST v Term)
term4 = RHS [ WfToken T.Lambda $ \_ -> RHS [ WfCall False (Call Iden () 0) $ \v -> lambda [v] ]
            , WfToken T.LParen $ \_ -> RHS [ WfCall False (Call Iden () 0) $ \v -> piOrSigma [v] ]
            , WfCall False (Call Term () 3) $ \v1 ->
                RHS [ WfToken T.Arrow $ \_ ->
                          RHS [ WfCall False (Call Term () 4) $ \v2 ->
                                    RHS [ Accept (Arr v1 v2) ] ]]
            ]
    where
      lambda nms = RHS [ WfToken T.FullStop $ \_ -> RHS [ WfCall False (Call Term () 4) $ \v -> RHS [ Accept (Lam (reverse nms) v) ] ]
                       , WfCall False (Call Iden () 0) $ \v -> lambda (v:nms)
                       ]

      piOrSigma nms = RHS [ WfToken T.Colon $ \_ ->
                            RHS [ WfCall False (Call Term () 4) $ \vt ->
                                  RHS [ WfToken T.RParen $ \_ ->
                                        RHS [ WfToken T.Arrow $ \_ ->
                                              RHS [ WfCall False (Call Term () 4) $ \vt' ->
                                                    RHS [ Accept (Pi (reverse nms) vt vt') ] ]
                                            , WfToken T.Times $ \_ ->
                                              RHS [ WfCall False (Call Term () 4) $ \vt' ->
                                                    RHS [ Accept (Sigma (reverse nms) vt vt') ] ]
                                            ]
                                      ]
                                ]
                          , WfCall False (Call Iden () 0) $ \v -> piOrSigma (v:nms)
                          ]
                  
-- ((Lam <$  terminal T.Lambda <*> nonEmptyList (call Iden) <* terminal T.FullStop <*> callAt Term 4)
--                 <|> (Pi
--                      <$  terminal T.LParen
--                      <*> nonEmptyList (call Iden)
--                      <*  terminal T.Colon
--                      <*> callAt Term 4
--                      <*  terminal T.RParen
--                      <*  terminal T.Arrow
--                      <*> callAt Term 4)
--                 <|> (Sigma
--                      <$  terminal T.LParen
--                      <*> nonEmptyList (call Iden)
--                      <*  terminal T.Colon
--                      <*> callAt Term 4
--                      <*  terminal T.RParen
--                      <*  terminal T.Times
--                      <*> callAt Term 4)
--                 <|> (Arr <$> callAt Term 3 <*  terminal T.Arrow <*> callAt Term 4))

term3 :: RHS NT T.Token v (AST v Term)
term3 = RHS [ WfCall False (Call Term () 2) $ \v ->
                  RHS [ WfToken T.Plus $ \_ -> RHS [ WfCall False (Call Term () 3) $ \v' -> RHS [ Accept (Sum v v') ] ]
                      , WfToken T.QuotePlus $ \_ -> RHS [ WfCall False (Call Term () 3) $ \v' -> RHS [ Accept (Desc_Sum v v') ] ]
                      ]
            ]

-- ((Sum <$> callAt Term 2 <*  terminal T.Plus <*> callAt Term 3)
--                 <|> (Desc_Sum <$> callAt Term 2 <* terminal T.QuotePlus <*> callAt Term 3))

term2 :: RHS NT T.Token v (AST v Term)
term2 = RHS [ WfCall False (Call Term () 1) $ \v ->
                  RHS [ WfToken T.Times $ \_ -> RHS [ WfCall False (Call Term () 2) $ \v' -> RHS [ Accept (Prod v v') ] ]
                      , WfToken T.QuoteTimes $ \_ -> RHS [ WfCall False (Call Term () 2) $ \v' -> RHS [ Accept (Desc_Prod v v') ] ]
                      ]
            ]

-- ((Prod <$> callAt Term 1 <*  terminal T.Times <*> callAt Term 2)
--                 <|> (Desc_Prod <$> callAt Term 1 <*  terminal T.QuoteTimes <*> callAt Term 2))

term1 :: RHS NT T.Token v (AST v Term)
term1 = RHS [ WfToken T.Inl $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (Inl v) ] ]
            , WfToken T.Inr $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (Inr v) ] ]
            , WfToken T.QuoteK $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (Desc_K v) ] ]
            , WfToken T.Mu $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (Mu v) ] ]
            , WfToken T.Construct $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (Construct v) ] ]
            , WfToken T.Quote_IId $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v -> RHS [ Accept (IDesc_Id v) ] ]
            , WfToken T.Quote_Sg $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v1 -> RHS [ WfCall False (Call Term () 0) $ \v2 -> RHS [ Accept (IDesc_Sg v1 v2) ] ] ]
            , WfToken T.Quote_Pi $ \_ -> RHS [ WfCall False (Call Term () 0) $ \v1 -> RHS [ WfCall False (Call Term () 0) $ \v2 -> RHS [ Accept (IDesc_Pi v1 v2) ] ] ]
            , WfCall False (Call Term () 0) $ \x ->
                RHS [ WfCall False (Call Term () 0) $ \y -> app x [y] ]
            ]
    where
      app x ys = RHS [ Accept (App x (reverse ys))
                     , WfCall False (Call Term () 0) $ \y -> app x (y:ys)
                     ]
                          


-- (Inl <$  terminal T.Inl <*> callAt Term 0)
--                 <|> (Inr <$  terminal T.Inr <*> callAt Term 0)
--                 <|> (Desc_K <$ terminal T.QuoteK <*> callAt Term 0)
--                 <|> (Mu     <$ terminal T.Mu <*> callAt Term 0)
--                 <|> (Construct <$ terminal T.Construct <*> callAt Term 0)
--                 <|> (IDesc_Id  <$ terminal T.Quote_IId <*> callAt Term 0)
--                 <|> (IDesc_Sg  <$ terminal T.Quote_Sg <*> callAt Term 0 <*> callAt Term 0)
--                 <|> (IDesc_Pi  <$ terminal T.Quote_Pi <*> callAt Term 0 <*> callAt Term 0)
--                 <|> (App <$> callAt Term 0 <*> nonEmptyList (callAt Term 0))

term0 :: RHS NT T.Token v (AST v Term)
term0 = (Proj1 <$ terminal T.Fst <*> callAt Term 0
                <|> (Proj2 <$ terminal T.Snd <*> callAt Term 0)
                <|> (MuI   <$ terminal T.MuI <*> callAt Term 0 <*> callAt Term 0)
                <|> (Induction <$ terminal T.Induction)
                <|> (Desc_Elim <$ terminal T.ElimD)
                <|> (UnitI     <$ terminal T.UnitValue)
                <|> (Pair  <$ terminal T.LDoubleAngle <*> (callTop Term) <* terminal T.Comma <*> (callTop Term) <* terminal T.RDoubleAngle)
                <|> (Case
                     <$  terminal T.Case
                     <*> (callTop Term)
                     <*  terminal T.For <*> call Iden <*  terminal T.FullStop <*> (callTop Term) <*  terminal T.With
                     <*  terminal T.LBrace
                     <*  terminal T.Inl <*> call Iden <* terminal T.FullStop <*> (callTop Term)
                     <*  terminal T.Semicolon
                     <*  terminal T.Inr <*> call Iden <* terminal T.FullStop <*> (callTop Term)
                     <*  terminal T.RBrace)
                <|> (Set <$ terminal T.Set <*> (pure 0 <|> (read . T.unpack <$> terminal T.Number)))
                <|> (Empty <$ terminal T.EmptyType)
                <|> (ElimEmpty <$ terminal T.ElimEmpty)
                <|> (Unit <$ terminal T.UnitType)
                <|> (Desc_Id <$ terminal T.QuoteId)
                <|> (Desc <$ terminal T.Desc)
                <|> (IDesc <$ terminal T.IDesc)
                <|> (IDesc_Elim <$ terminal T.IDesc_Elim)
                <|> (Var <$> call Iden)
                <|> (Paren <$ terminal T.LParen <*> (callTop Term) <* terminal T.RParen))