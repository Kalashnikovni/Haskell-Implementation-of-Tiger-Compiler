module RegAlloc where

import State
import TigerAbs as TA
import TigerAssem
import TigerCanon
import TigerColor
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

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy

import Data.Map as M
import Data.Set

import Prelude as P

import System.Directory 

main :: IO ()
main = 
  putStrLn "\n======= Test suite RegAlloc [] in progress =======" >>
  testerPrintDir "./test/test_code/good" >>
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
            (\x -> return $ x ++ [Proc (Label $ pack "final") (newFrame (pack "final") [])])
            res

instrSelectStage :: [TransFrag] -> EstadoTest ([Frag], [([Instr], Frame)]) 
instrSelectStage tfs =
  do let (strs, stms) = sepFrag tfs
     res <- mapM (\(stm, fr) -> do resCodeGen <- runMonada3 $ codeGen stm
                                   return $ either (error . show)
                                                   id
                                                   resCodeGen) stms
     return $ (strs, P.zip res (P.map snd stms))

regAllocStage :: ([Frag], [([Instr], Frame)]) -> EstadoTest ([Frag], [([Instr], Frame, Allocation)]) 
regAllocStage (strs, linstrs) =
  let res = P.map (\(instrs, fr) -> 
                    let newIns = procEntryExit2 fr instrs
                        (val, st) = runSt (runMonadaRA $ registerAllocation newIns fr) 0
                    in  either (error . show)
                               (\(is, f) -> (is, f, color $ snd val))
                               (fst val)) linstrs 
  in return $ (strs, res)
 
testerRegAlloc :: String -> EstadoTest ([Frag], [([Instr], Frame, Allocation)])
testerRegAlloc str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     res3 <- intermediateStage res2
     res4 <- instrSelectStage res3
     regAllocStage res4

--res::([Frag], [([Instr], Frame, Allocation)])
testerPrint :: String -> String -> IO ()
testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     let (res, st) = runSt (testerRegAlloc str) 0
     mapM_ (putStrLn . renderStrFrag) $ fst res
     mapM_ (\(instrs, fr, alloc) -> do --putStr $ renderInstr instrs fr
                                       putStrLn ""
                                       --putStrLn $ renderFrame fr
                                       putStrLn ""
                                       putStrLn $ show alloc
                                       putStrLn "") (snd res)

renderStrFrag :: Frag -> String
renderStrFrag (AString lab syms) =
  unpack lab ++ ":" ++ "\n  " ++ (concat $ P.map (\s -> unpack s ++ "\n  ") syms)
renderStrFrag _ = error "Fragments should be strings"


renderInstr :: [Instr] -> Frame -> String
renderInstr instrs fr =
  let ff = procEntryExit3 fr $ procEntryExit2 fr instrs
      renderInstrs ins = P.concat (P.map (format opmakestring) ins)
  in (renderInstrs $ prolog ff) ++ (renderInstrs $ body ff) ++ (renderInstrs $ epilogue ff)

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> 
                  testerPrint loc f >> 
                  putStrLn "***************") fs
