module TigerMakeGraph where

import TigerAssem as A

import Data.Graph
import Data.List as L
import Data.Map as Map

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Extra graphs & table methods -------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

type TableTemp = Map Int [ATemp]
type TableBool = Map Int Bool
type TableLabel = Map ALabel Int

gsucc :: Graph -> Vertex -> [Vertex]
gsucc g v = P.map snd $ P.filter (\(p, s) -> p == v) e
  where e = edges g

gpred :: Graph -> Vertex -> [Vertex]
gpred g v = P.map fst $ P.filter (\(p, s) -> s == v) e
  where e = edges g

adj :: Graph -> Vertex -> [Vertex]
adj g v = L.union (gpred g v) (gsucc g v)

newGraph :: Graph
newGraph = buildG (0, -1) []

newNode :: Graph -> Graph
newNode g = buildG (0, length (vertices g)) (edges g) 

mkEdge :: Graph -> Edge -> Graph
mkEdge g e = buildG (0, length (vertices g) - 1) (e : edges g)

rmEdge :: Graph -> Edge -> Graph
rmEdge g e = buildG (0, length (vertices g) - 1) (edges g L.\\ [e]) 

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Control flow graphs ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data FlowGraph = FGraph {control :: Graph,
                         def :: TableTemp,
                         use :: TableTemp,
                         ismove :: TableBool,
                         labmap :: TableLabel}
  deriving Show

instrs2graph :: [Instr] -> (FlowGraph, [Vertex])
instrs2graph [] = (FGraph{control = newGraph, 
                          def = Map.empty, 
                          use = Map.empty, 
                          ismove = Map.empty,
                          labmap = Map.empty}, [])
instrs2graph ((Oper a dst src j) : instrs) =
  (fres{control = addJEdges j node (labmap fres) $ mkEdge (newNode $ control fres) (node, vres !! 0), 
        def = Map.insert node dst (def fres), 
        use = Map.insert node src (use fres), 
        ismove = Map.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = length vres + 1
instrs2graph ((Move a dst src) : instrs) = 
  (fres{control = mkEdge (newNode $ control fres) (node, vres !! 0), 
        def = Map.insert node dst (def fres), 
        use = Map.insert node src (use fres), 
        ismove = Map.insert node True $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = length vres + 1
instrs2graph ((ILabel a l) : instrs) =
  (fres{labmap = Map.insert l (vres !! 0) $ labmap fres}, vres)
  where (fres, vres) = instrs2graph instrs


addJEdges :: Maybe [ALabel] -> Vertex -> TableLabel -> Graph -> Graph
addJEdges Nothing _ _ g = g
addJEdges (Just []) _ _ g = error "Revisar hasta liveness -- TigerMakeGraph" 
addJEdges (Just j) v tl g = 
  L.foldl (\g' j' -> case Map.lookup j' tl of
                       Just jj -> mkEdge g' (v, jj)
                       Nothing -> error "Revisar hasta liveness -- TigerMakeGraph") g j   





