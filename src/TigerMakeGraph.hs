module TigerMakeGraph where

import TigerAssem as A

import Data.Graph
import Data.GraphViz.Attributes.Complete as Att
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import qualified Data.Text.Lazy as Lazy
import Data.List as L
import Data.Map as Map
import Data.Set as Set

import Debug.Trace

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
        def = Map.insert node [] $ def fres,
        labmap = if L.null vres then labmap fres
                   else Map.insert l node $ labmap fres,
        use = Map.insert node [] $ use fres,
        ismove = Map.insert node False $ ismove fres}, node : vres)
  where (fres, vres) = instrs2graph instrs
        node = L.length vres

i2gWithJumps :: [Instr] -> (FlowGraph, [Vertex]) -> [Vertex] -> (FlowGraph, [Vertex])
i2gWithJumps [] (f, vs) vlist = (f, vlist)
i2gWithJumps ((Oper a dst src j) : instrs) (f, vs) vlist =
  i2gWithJumps instrs (f{control = addJEdges j (head vs) (labmap f) $ control f}, tail vs) vlist
i2gWithJumps (i : instrs) (f, vs) vlist = i2gWithJumps instrs (f, tail vs) vlist

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

-- Print graph in .dot format. For debugging purposes
defaultVis :: FlowGraph -> Lazy.Text
defaultVis fg =
  let (vs, es) = (vertices $ control fg, edges $ control fg)  
      (vcount, dotvs) = 
        P.foldl (\(i, vlist) v -> (i + 1, 
                                   DotNode i [Att.Label $ 
                                              StrLabel $ 
                                              Lazy.pack $ maybe (error "Liveness.hs")
                                                                show
                                                                (Map.lookup v $ info fg)] : vlist)) 
                              (0, []) vs
      (ecount, dotes) = P.foldl (\(i, elist) e -> (i + 1, DotEdge (fst e) (snd e) 
                                                          [Att.Label $ StrLabel $ Lazy.pack $ show e] : elist)) 
                              (0, []) es
  in printDotGraph $
       DotGraph True True (Just $ Str (Lazy.pack "FlowGraph")) (DotStmts [] [] dotvs dotes)
