module InstrSelect where

import State
import TigerAbs as TA
import TigerAssem
import TigerCanon
import TigerEscap
import TigerFrame hiding (exp)
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

import Prelude as P

import System.Directory 

main :: IO ()
main = 
  putStrLn "\n======= Test suite Instruction Selection [] in progress =======" >>
  testerPrintDir "./test/test_code/good" >>
  putStrLn "\n======= Test suite FIN ======="  

type EstadoTest = StGen

runMonadaCanon :: Tank [Stm] -> StGen ([Stm], TAM)
runMonadaCanon w = runStateT w firstTank

parseStage :: String -> EstadoTest TA.Exp
parseStage str =
  either (error . show)
         return
         (parse str)

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
     return $ (strs, zip res (map snd stms))

testerInstrSelect :: String -> EstadoTest ([Frag], [([Instr], Frame)])
testerInstrSelect str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     res3 <- intermediateStage res2 
     instrSelectStage res3

testerPrint :: String -> String -> IO ()
testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     let res = fst $ runSt (testerInstrSelect str) 0
     mapM_ (putStrLn . renderStrFrag) $ fst res
     mapM_ (\(instrs, fr) -> do putStr $ renderInstr instrs fr
                                putStrLn $ show fr
                                putStrLn "") $ snd res

renderStrFrag :: Frag -> String
renderStrFrag (AString lab syms) =
  unpack lab ++ ":" ++ "\n  " ++ (concat $ map (\s -> unpack s ++ "\n  ") syms)
renderStrFrag _ = error "Fragments should be strings"

renderInstr :: [Instr] -> Frame -> String
renderInstr instrs fr =
  let ff = procEntryExit3 fr $ procEntryExit2 fr instrs
      renderInstrs ins = P.concat (P.map (format opmakestring) ins)
  in (renderInstrs $ prolog ff) ++ (renderInstrs $ body ff) ++ (renderInstrs $ epilogue ff)

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
