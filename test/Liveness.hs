module Liveness where

import State
import TigerAbs as TA
import TigerAssem
import TigerCanon
import TigerEscap
import TigerFrame hiding (exp)
import TigerLiveness
import TigerMakeGraph
import TigerMunch
import TigerParser (parse)
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTrans
import TigerTree
import TigerUnique
import Tools

import Control.Monad.Trans.State.Lazy

import Data.Graph
import Data.GraphViz.Attributes.Complete as Att
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical
import Data.Text.Lazy as Lazy
import Data.Map as M
import Data.Set

import Debug.Trace

import Prelude as P

import System.Directory 

main :: IO ()
main = 
  putStrLn "\n======= Test suite Liveness [] in progress =======" >>
  testerPrintDir "./test/test_code/good/fortry" >>
  putStrLn "\n======= Test suite FIN ======="  

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

flowGraph :: ([Frag], [([Instr], Frame)]) -> EstadoTest (FlowGraph, [Vertex])
flowGraph (_, stms) = 
  let instrs = P.concat $ P.map (\(ins, fr) -> procEntryExit2 fr ins) stms
      (fg, vs) = instrs2graph instrs
  in return $ i2gWithJumps instrs (fg, vs) vs

livenessStage :: (FlowGraph, [Vertex]) -> EstadoTest (Map Vertex (Set ATemp, Set ATemp)) 
livenessStage (fg, vs) =
  do let res = inout $ snd $ runState (setEquationAlgorithm fg vs) 
                                      (initLEstado{isSame = M.fromList $ P.map (\v -> (v, False)) vs}) 
     return res

testerLiveness :: String -> EstadoTest (FlowGraph, Map Vertex (Set ATemp, Set ATemp))
testerLiveness str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     res3 <- intermediateStage res2
     res4 <- instrSelectStage res3
     (fg, vs) <- flowGraph res4
     liv <- livenessStage (fg, vs)
     return (fg, liv)

testerPrint :: String -> String -> String -> IO ()
testerPrint loc f out =
  do str <- readFile $ loc ++ '/' : f
     let (res, st) = runSt (testerLiveness str) 0
     writeFile out (Lazy.unpack $ defaultVis $ fst res)
     printMap $ snd res
     --printMap $ use $ fst res
     --printMap $ def $ fst res

printMap :: (Show a, Show b) => Map a b -> IO ()
printMap m =
  mapM_ (\(k, v) -> putStrLn $ show k ++ ": " ++ show v) (M.toList m) 

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> 
                  testerPrint loc f  ("./test/FG/" ++ (fst $ P.span (/= '.') f) ++ ".dot") >> 
                  putStrLn "***************") fs
