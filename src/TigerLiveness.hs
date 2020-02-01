module TigerLiveness where

import TigerAssem
import TigerErrores
import TigerMakeGraph
import TigerSymbol
import TigerTemp
import TigerUnique

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Graph
import Data.List as L
import Data.Map as Map
import Data.Set as Set

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Interference Graphs ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

type TableNode = Map ATemp Vertex
type TableITemp = Map Vertex ATemp

data IGraph = IG {graph :: Graph,
                  tnode :: TableNode,
                  gtemp :: TableITemp,
                  moves :: [(Vertex, Vertex)]} -- El primer elemento de la tupla es el destino, y el segundo el fuente
  deriving Show

type LiveSet = (Map Temp (), [Temp])
type LiveMap = Map Vertex LiveSet 

interferenceGraph :: FlowGraph -> (IGraph, Map Vertex (Set Temp))  
interferenceGraph fg = 
  let igres = P.foldl (\ig n -> interferenceAux n (def fg) livemap ig) initIGraph (vertices $ control fg) 
  in (igres, Map.map snd live) 
  where live = inout $ snd $ runState (setEquationAlgorithm fg (vertices $ control fg)) initLEstado 
        livemap = Map.map (\(i, o) -> (Map.fromSet (\x -> ()) o, Set.toList o)) live
        nodes = Set.toList $ Set.fromList $ concat $ Map.elems $ Map.unionWith (++) (def fg) (use fg)
        naux = P.length nodes - 1
        verts = [0..naux]
        gtemps = Map.fromList $ zip verts nodes 
        tnodes = Map.fromList $ zip nodes verts
        movesFG = Map.elems $ restrictKeys (info fg) $ Set.fromList $ keys $ Map.filter (== True) $ ismove fg
        movesSrc = P.map (\t -> lookInGraph 5 id $ Map.lookup t tnodes) $ concat $ P.map src movesFG
        movesDst = P.map (\t -> lookInGraph 6 id $ Map.lookup t tnodes) $ concat $ P.map dst movesFG
        initIGraph = IG{graph = buildG (0, naux) [],
                        tnode = tnodes,
                        gtemp = gtemps,
                        moves = zip movesDst movesSrc}

lookInGraph :: Int -> (a -> b) -> Maybe a -> b  
lookInGraph i f v = maybe (error $ "Revisar hasta Liveness -- TigerLiveness " ++ show i) f v

-- Vertice en cuestion, definiciones, livemap 
interferenceAux :: Vertex -> TableTemp -> LiveMap -> IGraph -> IGraph
interferenceAux v defs lm ig =
  lookInGraph 1 (\lt -> P.foldl (\gr t -> interfLm lm t v gr) ig lt) (Map.lookup v defs)

interfLm :: LiveMap -> Temp -> Vertex -> IGraph -> IGraph
interfLm lm t v ig = 
  lookInGraph 2 (\(mt, lt) -> 
                  P.foldl (\gr temp -> 
                            lookInGraph 3 (\vdef -> 
                                            lookInGraph 4 (\vlm -> addIEdge vdef vlm ig) 
                                                          (Map.lookup temp nodes)) 
                                          (Map.lookup t nodes)) ig lt) 
                (Map.lookup v lm)
  where nodes = tnode ig


addIEdge :: Vertex -> Vertex -> IGraph -> IGraph
addIEdge v1 v2 ig = 
  ig{graph = mkEdge (graph ig) (v1, v2)}
