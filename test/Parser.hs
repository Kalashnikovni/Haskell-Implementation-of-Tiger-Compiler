import TigerAbs as TA
import TigerParser as P
import TigerPretty
import TigerUnique
import Tools

import State

import System.Directory

import Text.Parsec

main :: IO ()
main =
  putStrLn "\n======= Test PARSER in progress =======" >>
  --putStrLn "Print results, good:" >>
  --testerPrintDir "./test/test_code/good" >>
  --putStrLn "Print results, type:" >>
  --testerPrintDir "./test/test_code/type" >>
  --putStrLn "Bad:" >>
  --testerPrintDir "./test/test_code/syntax" >>
  testerPrint "./test/test_code/good" "merge.tig" show >>
  putStrLn "\n======= Test FIN ======="

type EstadoTest = StGen

parseStage :: String -> EstadoTest Exp
parseStage str =
  either (error . show)
         return
         (P.parse str)

testerParser :: String -> EstadoTest TA.Exp
testerParser str = parseStage str

-- Si queremos ver el resultado del parseo
-- Sencillamente lo bindeamos en el main de este archivo:
-- ... >>
-- testerPrint "location" >>
-- ...
testerPrint :: String -> String -> (Exp -> String) -> IO ()
testerPrint loc f printer =
  do str <- readFile $ loc ++ '/' : f
     putStrLn $ printer $ fst $ runSt (testerParser str) 0

-- Ahora para testear printeando un directorio entero
testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f renderExp >> putStrLn "***************") fs
