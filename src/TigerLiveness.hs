module TigerLiveness where

import TigerAssem
import TigerMakeGraph
import TigerTemp

import Algebra.Graph.AdjacencyMap as Adj

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Graph
import Data.List as L
import Data.Map as M
import Data.Matrix as Mat
import Data.Set as S

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Liveness analysis ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

setEquationAlgorithm :: FlowGraph -> [Vertex] -> LiveMonada ()
setEquationAlgorithm fg vs =
  do mapM_ (seaAux fg) vs
     st <- get
     let same = and $ P.map snd (M.toList $ isSame st)
     case same of
       True  -> return ()
       False -> setEquationAlgorithm fg vs

seaAux :: FlowGraph -> Vertex -> LiveMonada ()
seaAux fg v = 
  do st <- get
     let io = inout st
     let oldin = maybe S.empty fst $ M.lookup v io
     let oldout = maybe S.empty snd $ M.lookup v io
     let usev = maybe S.empty S.fromList $ M.lookup v $ use fg 
     let defv = maybe S.empty S.fromList $ M.lookup v $ def fg
     let newin = S.union usev (oldout S.\\ defv)
     let newout = M.foldl S.union S.empty (M.map fst $ restrictKeys io (S.fromList $ gsucc (control fg) v)) 
     case (newin == oldin, newout == oldout) of
       (True, True) -> put st{inout = M.insert v (newin, newout) io,
                              isSame = M.insert v True $ isSame st}
       _            -> put st{inout = M.insert v (newin, newout) io,
                              isSame = M.insert v False $ isSame st}
  where errSeaAux ver tab = error $ "El vertice " ++ show ver ++ " tendria que estar en el mapeo " ++ 
                                    show tab ++ "-- TigerLiveness" 

data LiveEstado = LEst {inout :: Map Vertex (Set ATemp, Set ATemp), 
                        isSame :: Map Vertex Bool} deriving Show
initLEstado = LEst {inout = M.empty, isSame = M.empty}
type LiveMonada = State LiveEstado

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Interference Graphs ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

max32Int :: Int
max32Int = 2147483647

type TempGraph = AdjacencyMap ATemp
type Bit = Bool

-- Con esta representacion construimos el grafo,
-- y una vez hecho llamamos a las funciones que hacen falta 
-- para guardar la lista de adyacencia y la bit matrix
data IGraph = IG {graph :: TempGraph, 
                  bitMatrix :: Matrix Bit,
                  degree :: Map ATemp Int,
                  edgesG :: [(ATemp, ATemp)],
                  adjSet :: Set (ATemp, ATemp)}
  deriving Show

defaultIGraph :: IGraph
defaultIGraph = IG{graph = Adj.empty,
                   bitMatrix = matrix 0 0 (const False),
                   degree = M.empty,
                   edgesG = [],
                   adjSet = S.empty}

-- A es adyacente a B si hay un arco partiendo de A,
-- y llegando a B
buildBitMatrix :: TempGraph -> Matrix Bit
buildBitMatrix graph = 
  let adjList = sort $ adjacencyList graph
  in matrix nm nm (\(n, m) -> L.elem (fst $ adjList !! (m - 1)) $ snd $ adjList !! (n - 1))
  where nm = S.size $ vertexSet graph
