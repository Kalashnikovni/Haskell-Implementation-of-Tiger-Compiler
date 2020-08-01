module Stages (parseStage, 
               escapStage, 
               formatInstr, 
               intermediateStage, 
               instrSelectStage, 
               renderStrFrag,
               renderInstr,
               flowGraph,
               livenessStage,
               defaultVis,
               printMap,
               regAllocStage) where

import State
import TigerAbs as TA
import TigerAssem
import TigerCanon
import TigerColor
import TigerEscap
import TigerFrame
import TigerLiveness
import TigerMakeGraph
import TigerMunch
import TigerParser
import TigerSeman
import TigerSymbol as Sym
import TigerTrans
import TigerTree
import TigerUnique as U

import Control.Monad.Trans.State.Lazy

import Data.Graph
import Data.GraphViz.Attributes.Complete as Att
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import Data.Map as M
import Data.Text.Lazy as Lazy
import Data.Set

import Prelude as P

type EstadoTest = StGen

runMonadaCanon :: Tank [Stm] -> StGen ([Stm], TAM)
runMonadaCanon w = runStateT w firstTank

parseStage :: String -> EstadoTest TA.Exp
parseStage str =
  either (error . show)
         return
         (TigerParser.parse str)

escapStage :: TA.Exp -> EstadoTest TA.Exp
escapStage exp =
  either (error . show)
         return
         (calcularEEsc exp)

intermediateStage :: TA.Exp -> EstadoTest [TransFrag]
intermediateStage exp =
  do res <- runTransProg exp
     either (error . show)
            return
            res

instrSelectStage :: [TransFrag] -> EstadoTest ([Frag], [([Instr], Frame)]) 
instrSelectStage tfs =
  do let (strs, stms) = sepFrag tfs
     res <- mapM (\(stm, fr) -> do resCodeGen <- runMonada3 $ codeGen stm
                                   return $ either (error . show)
                                                   id
                                                   resCodeGen) stms
     return $ (strs, P.zip res (P.map snd stms))

renderStrFrag :: Frag -> String
renderStrFrag (AString lab syms) =
  Sym.unpack lab ++ ":" ++ "\n  " ++ (P.concat $ P.map (\s -> Sym.unpack s ++ "\n  ") syms)
renderStrFrag _ = error "Fragments should be strings"

renderInstr :: [Instr] -> Frame -> String
renderInstr instrs fr =
  let ff = procEntryExit3 fr $ procEntryExit2 fr instrs
      renderInstrs ins = P.concat (P.map (format opmakestring) ins)
  in (renderInstrs $ prolog ff) ++ (renderInstrs $ body ff) ++ (renderInstrs $ epilogue ff)

formatInstr :: Instr -> String
formatInstr = format opmakestring

flowGraph :: ([Frag], [([Instr], Frame)]) -> EstadoTest (FlowGraph, [Vertex])
flowGraph (_, stms) = 
  let instrs = P.concat $ P.map (\(ins, fr) -> procEntryExit2 fr ins) stms
      (fg, vs) = instrs2graph instrs
  in return $ i2gWithJumps instrs (fg, vs) vs

printMap :: Map Vertex (Set ATemp, Set ATemp) -> IO ()
printMap m =
  mapM_ (\(k, v) -> putStrLn $ show k ++ ": " ++ show v) (M.toList m) 

livenessStage :: (FlowGraph, [Vertex]) -> EstadoTest (Map Vertex (Set ATemp, Set ATemp)) 
livenessStage (fg, vs) =
  do let res = inout $ snd $ runState (setEquationAlgorithm fg vs) 
                                      (initLEstado{isSame = M.fromList $ P.map (\v -> (v, False)) vs}) 
     return res

regAllocStage :: Integer -> ([Frag], [([Instr], Frame)]) -> EstadoTest ([Frag], [([Instr], Frame, Allocation)])
regAllocStage st (strs, linstrs) =
  let res = P.map (\(instrs, fr) -> 
                    let newIns = procEntryExit2 fr instrs
                        (val, st') = runSt (runMonadaRA $ registerAllocation newIns fr) st
                    in  either (error . show)
                               (\(is, f) -> (is, f, color $ snd val))
                               (fst val)) linstrs 
  in return $ (strs, res)
