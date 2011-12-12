{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Language.Forvie.Parsing.FGL where

import           Control.Monad.State
import           Control.Monad.Writer

import           Language.Forvie.Parsing.Parser

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS (reachable)
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.List (intercalate)
import qualified Data.Set as S

--------------------------------------------------------------------------------
newtype GRef a = GR { unGR :: Int }

data NodeRepresentationPart
    = Text String
    | Ref  Int

instance Show NodeRepresentationPart where
    show (Text s) = s
    show (Ref i)  = show i

type NodeRepresentation = [NodeRepresentationPart]

class ToNodeRepresentation f where
    toNodeRep :: f GRef a -> NodeRepresentation

data SPPFNode
    = AmbigNode Int Int
    | ParseNode Int NodeRepresentation

--------------------------------------------------------------------------------
newtype GraphParseMonad m a =
    GPM { unGPM :: StateT Int (WriterT ([LNode SPPFNode], [LEdge ()]) m) a }
    deriving (Monad, Functor)

addEdge :: Monad m => Int -> Int -> GraphParseMonad m ()
addEdge n1 n2 = GPM $ tell ([], [(n1,n2,())])

addNode :: Monad m => SPPFNode -> GraphParseMonad m Int
addNode x = GPM $ do n <- get
                     modify (+1)
                     tell ([(n,x)], [])
                     return n

instance (Monad m, ToNodeRepresentation f) => ParseResultsMonad f (GraphParseMonad m) where
    type ResultNode f (GraphParseMonad m) = GRef

    newResult i j x = do
      n <- addNode (AmbigNode i j)
      let rep = toNodeRep x
      n' <- addNode (ParseNode n rep)
      addEdge n n'
      forM_ rep $ \p -> case p of
                          Text _ -> return ()
                          Ref i  -> addEdge n' i
      return (GR n)

    addResult (GR n) x = do
      let rep = toNodeRep x
      n' <- addNode (ParseNode n rep)
      addEdge n n'
      forM_ rep $ \p -> case p of
                          Text _ -> return ()
                          Ref i  -> addEdge n' i

toGr :: Monad m => GraphParseMonad m (Maybe (GRef a)) -> m (Maybe (Gr SPPFNode ()))
toGr p = do
  (maybeRoot, (nodesG, edgesG)) <- runWriterT (evalStateT (unGPM p) 0)
  case maybeRoot of
    Nothing -> return Nothing
    Just (GR root) -> do
      let gr          = mkGraph nodesG edgesG
          unreachable = S.fromList (nodes gr) `S.difference` (S.fromList $ reachable root gr)
      return $ Just (delNodes (S.elems unreachable) gr)

toGraphViz :: Graph gr => gr SPPFNode el -> DotGraph Node
toGraphViz = graphToDot params
    where params = Params { isDirected       = True
                          , globalAttributes = [ GraphAttrs [ RankDir FromLeft
                                                            , RankSep [1]
                                                            ]
                                               ]
                          , clusterBy        = clusterByF
                          , clusterID        = clusterIDF
                          , fmtCluster       = const [ GraphAttrs [ BgColor (RGB 0xcc 0xcc 0xcc)
                                                                  , RankSep [0.2]
                                                                  ]
                                                     ]
                          , fmtNode          = nodeFormatter
                          , fmtEdge          = const []
                          }

          clusterByF (n, AmbigNode i j )   = C n $ N (n, AmbigNode i j)
          clusterByF (n, ParseNode p rep)  = C p $ N (n, ParseNode p rep)

          clusterIDF = Int

          nodeFormatter (n, AmbigNode i j)   = [ toLabel $ show n ++ " (" ++ show i ++ ".." ++ show j ++ ")"
                                               , Shape DiamondShape
                                               ]
          nodeFormatter (n, ParseNode _ rep) = [ toLabel $ intercalate " " $ map show rep
                                               , Style [SItem Filled []]
                                               , FillColor (RGBA 255 255 255 255)
                                               , Shape BoxShape
                                               ]
