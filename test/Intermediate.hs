import TigerAbs
import TigerEscap
import TigerParser (parse)
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTrans
import TigerUnique
import Tools

import State

import System.Directory


main :: IO ()
main = 
  putStrLn "\n======= Test suite Translate [for TigerTrans testing] in progress =======" >>
  putStrLn "Show results good:" >>
  testerPrintDir "./test/test_code/good" >>
  putStrLn "\n======= Test suite FIN ======="

type EstadoTest = StGen

parseStage :: String -> EstadoTest Exp
parseStage str =
  either (error . show)
         return
         (parse str)

escapStage :: Exp -> EstadoTest Exp
escapStage exp =
  either (error . show)
         return
         (calcularEEsc exp)

intermediateStage :: Exp -> EstadoTest [TransFrag]
intermediateStage exp =
  do res <- runTransProg exp
     either (error . show)
            return
            res

testerIntermediate :: String -> EstadoTest [TransFrag]
testerIntermediate str =
  do res1 <- parseStage str
     res2 <- escapStage res1
     intermediateStage res2

testerPrint :: String -> String -> IO ()
testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     mapM_ (\frag -> putStrLn $ renderFrag frag) (fst $ runSt (testerIntermediate str) 0) 

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
