module TigerLiveness where

import TigerAssem
import TigerTemp

import Algebra.Graph.AdjacencyMap as Adj

import Data.List as L
import Data.Map as M
import Data.Matrix as Mat
import Data.Set as S

import Prelude as P

max32Int :: Int
max32Int = 2147483647

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Interference Graphs ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

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
