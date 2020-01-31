
module TigerMakeGraph where

import TigerAssem as A

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Graph
import Data.List as L
import Data.Map as Map
import Data.Set as Set

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Extra graphs & table methods -------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

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

type TableInstr = Map Vertex Instr
type TableTemp = Map Vertex [ATemp]
type TableBool = Map Vertex Bool
type TableLabel = Map ALabel Vertex

data FlowGraph = FGraph {control :: Graph,
                         info :: TableInstr,
                         def :: TableTemp,
                         use :: TableTemp,
                         ismove :: TableBool,
                         labmap :: TableLabel}
  deriving Show

instrs2graph :: [Instr] -> (FlowGraph, [Vertex])
instrs2graph [] = (FGraph{control = newGraph,
                          info = Map.empty, 
                          def = Map.empty, 
                          use = Map.empty, 
                          ismove = Map.empty,
                          labmap = Map.empty}, [])
instrs2graph (o@(Oper a dst src j) : instrs) =
  --(fres{control = addJEdges j node (labmap fres) $ mkEdge (newNode $ control fres) (node, vres !! 0), 
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0), 
        info = Map.insert node o $ info fres, 
        def = Map.insert node dst $ def fres, 
        use = Map.insert node src $ use fres, 
        ismove = Map.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = L.length vres
instrs2graph (m@(Move a dst src) : instrs) = 
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0), 
        info = Map.insert node m $ info fres,
        def = Map.insert node dst $ def fres, 
        use = Map.insert node src $ use fres, 
        ismove = Map.insert node True $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = L.length vres
instrs2graph (lb@(ILabel a l) : instrs) =
  (fres{control = if L.null vres then newNode $ control fres
                    else mkEdge (newNode $ control fres) (node, vres !! 0),
        info = Map.insert node lb $ info fres, 
        labmap = if L.null vres then labmap fres
                   else Map.insert l node $ labmap fres,
        ismove = Map.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = L.length vres

i2gWithJumps :: [Instr] -> (FlowGraph, [Vertex]) -> (FlowGraph, [Vertex])
i2gWithJumps [] (f, vs) = (f, vs)
i2gWithJumps ((Oper a dst src j) : instrs) (f, vs) =
  i2gWithJumps instrs (f{control = addJEdges j (head vs) (labmap f) $ control f}, tail vs)
i2gWithJumps (i : instrs) (f, vs) = i2gWithJumps instrs (f, tail vs) 

addJEdges :: Maybe [ALabel] -> Vertex -> TableLabel -> Graph -> Graph
addJEdges Nothing _ _ g = g
addJEdges (Just []) _ _ g = error "Revisar hasta liveness 1 -- TigerMakeGraph" 
addJEdges (Just j) v tl g = 
  L.foldl (\g' j' -> case Map.lookup j' tl of
                       Just jj -> let eds = edges g'
                                  in case elem (v, jj) eds of
                                       True  -> g' 
                                       False -> mkEdge g' (v, jj)
                       Nothing -> g') g j   

setEquationAlgorithm :: FlowGraph -> [Vertex] -> LiveMonada ()
setEquationAlgorithm fg vs =
  mapM_ (seaAux fg) vs

seaAux :: FlowGraph -> Vertex -> LiveMonada ()
seaAux fg v = 
  do st <- get
     let io = inout st
     --let oldin = maybe (errSeaAux v io) fst $ Map.lookup v io
     --let oldout = maybe (errSeaAux v io) snd $ Map.lookup v io
     let oldin = maybe Set.empty fst $ Map.lookup v io
     let oldout = maybe Set.empty snd $ Map.lookup v io
     let usev = maybe Set.empty Set.fromList $ Map.lookup v $ use fg 
     let defv = maybe Set.empty Set.fromList $ Map.lookup v $ def fg
     let newin = Set.union usev (oldout Set.\\ defv)
     let newout = Map.foldl Set.union Set.empty (Map.map fst $ restrictKeys io (Set.fromList $ gsucc (control fg) v)) 
     case (newin == oldin, newout == oldout) of
       (True, True) -> return ()
       _            -> put st{inout = Map.insert v (newin, newout) io}
  where errSeaAux ver tab = error $ "El vertice " ++ show ver ++ " tendria que estar en el mapeo " ++ 
                                    show tab ++ "-- TigerLiveness" 

data LiveEstado = LEst {inout :: Map Vertex (Set ATemp, Set ATemp)} deriving Show
initLEstado = LEst {inout = Map.empty}
type LiveMonada = State LiveEstado
