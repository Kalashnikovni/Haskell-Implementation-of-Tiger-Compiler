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

instrSelectStageStm :: [TransFrag] -> EstadoTest [([Instr], Frame)] 
instrSelectStageStm tfs =
  do let (strs, stms) = sepFrag tfs
     res <- mapM (\(stm, _) -> do resCodeGen <- runMonada3 $ codeGen stm
                                  return $ either (error . show)
                                                  id
                                                  resCodeGen) stms
     return $ zip res (map snd stms)

testerInstrSelect :: String -> EstadoTest [([Instr], Frame)]
testerInstrSelect str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     res3 <- intermediateStage res2 
     instrSelectStageStm res3

testerPrint :: String -> String -> IO ()
testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     putStrLn $ show $ fst $ runSt (testerInstrSelect str) 0

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
