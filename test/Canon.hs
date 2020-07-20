import TigerAbs as TA
import TigerCanon
import TigerEscap
import TigerFrame
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTrans
import TigerTree
import TigerUnique
import Tools

import Control.Monad.Trans.State.Lazy

import State

import System.Directory

import TigerParser (parse)

main :: IO ()
main = 
  putStrLn "\n======= Test suite Translate [for TigerTrans testing] in progress =======" >>
  --putStrLn "Show results good:" >>
  testerPrint "./test/test_code/good" "test06.tig" >>
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

canonStage :: [TransFrag] -> EstadoTest [([Stm], Frame)]
canonStage tfs =
  do let (strs, stms) = sepFrag tfs
     res <- mapM (\s -> do res <- runMonadaCanon . canonM . fst $ s
                           return $ fst res) stms
     return $ zip res (map snd stms)

testerCanon :: String -> EstadoTest [([Stm], Frame)]
testerCanon str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     res3 <- intermediateStage res2
     canonStage res3

testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     mapM_ (\(stms, fr) -> putStrLn $ show stms) (fst $ runSt (testerCanon str) 0) 

testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
