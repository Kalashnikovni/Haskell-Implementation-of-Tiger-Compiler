module TigerLiveness where

import TigerAssem
import TigerSymbol
import TigerTemp
import TigerUnique

import Algebra.Graph.AdjacencyMap as Adj

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.List as L
import Data.Map as Map
import Data.Set as Set

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Interference Graphs ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

type TableNode = Map ATemp Vertex
type TableITemp = Map Vertex ATemp

-- Con esta representacion construimos el grafo,
-- y una vez hecho llamamos a las funciones que hacen falta 
-- para guardar la lista de adyacencia y la bit matrix
data IGraph = IG {graph :: AdjacencyMap ATemp  
                  tnode :: TableNode,
                  gtemp :: TableITemp,
                  moves :: [(Vertex, Vertex)], -- El primer elemento de la tupla es el destino, y el segundo el fuente
                  }
  deriving Show

type LiveSet = (Map Temp (), [Temp])
type LiveMap = Map Vertex LiveSet 

{-
interferenceGraph :: FlowGraph -> (IGraph, Map Vertex (Set Temp))  
interferenceGraph fg = 
  let igres = P.foldl (\ig n -> interferenceAux n (def fg) livemap ig) initIGraph (vertices $ control fg) 
  in (igres, Map.map snd live) 
  where live = inout $ snd $ runState (setEquationAlgorithm fg (vertices $ control fg)) initLEstado 
        livemap = Map.map (\(i, o) -> (Map.fromSet (\x -> ()) o, Set.toList o)) live
        nodes = Set.toList $ Set.fromList $ concat $ Map.elems $ Map.unionWith (++) (def fg) (use fg)
        lengthNodes = P.length nodes
        naux = lengthNodes - 1
        verts = [0..naux]
        gtemps = Map.fromList $ zip verts nodes 
        tnodes = Map.fromList $ zip nodes verts
        movesFG = Map.elems $ restrictKeys (info fg) $ Set.fromList $ keys $ Map.filter (== True) $ ismove fg
        movesSrc = P.map (\t -> lookInGraph 5 id $ Map.lookup t tnodes) $ concat $ P.map src movesFG
        movesDst = P.map (\t -> lookInGraph 6 id $ Map.lookup t tnodes) $ concat $ P.map dst movesFG
        initIGraph = IG{graph = buildG (0, naux) [],
                        tnode = tnodes,
                        gtemp = gtemps,
                        moves = zip movesDst movesSrc,
                        adjLists = Map.empty,
                        bitMatrix = matrix lengthNodes lengthNodes (\_ -> False)}

lookInGraph :: Int -> (a -> b) -> Maybe a -> b  
lookInGraph i f v = maybe (error $ "Revisar hasta Liveness -- TigerLiveness " ++ show i) f v

-- Vertice en cuestion, definiciones, livemap 
interferenceAux :: Vertex -> TableTemp -> LiveMap -> IGraph -> IGraph
interferenceAux v defs lm ig =
  lookInGraph 1 (\lt -> P.foldl (\gr t -> interfLm lm t v gr) ig lt) (Map.lookup v defs)

interfLm :: LiveMap -> Temp -> Vertex -> IGraph -> IGraph
interfLm lm t v ig = 
  lookInGraph 2 
    (\(mt, lt) -> 
     P.foldl (\gr temp -> 
              lookInGraph 3 (\vdef -> 
                             lookInGraph 4 (\vlm -> case isMove vdef vlm of
                                                      False -> addIEdge vdef vlm ig
                                                      True -> if vlm /= vdef then addIEdge vdef vlm ig
                                                                else ig) 
                                          (Map.lookup temp nodes)) 
                            (Map.lookup t nodes)) ig lt) 
    (Map.lookup v lm)
  where nodes = tnode ig
        movs = moves ig
        isMove x y = elem (x, y) movs 


addIEdge :: Vertex -> Vertex -> IGraph -> IGraph
addIEdge v1 v2 ig = 
  ig{graph = mkEdge (graph ig) (v2, v1),
     adjLists = Map.insert v2 (lookInGraph 7 (++ [v1]) (Map.lookup v2 $ adjLists ig)) $ adjLists ig,
     bitMatrix = setElem True (v1 + 1, v2 + 1) $ bitMatrix ig}
-}
