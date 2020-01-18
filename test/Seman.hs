import Data.Either
import State
import TigerEscap
import TigerParser (parse)
import TigerSeman
import TigerSymbol
import Tools
import System.Directory

-- *** Test suite pensada para evaluar la primera etapa del compilador.
-- *** Es decir, principalmente el chequeo de tipos de expresiones.
-- *** En la segunda etapa metemos mano de lo que hicimos en la primera
-- *** etapa, así que esta test suite pierde un poco el sentido.

main :: IO ()
main = 
  putStrLn "\n======= Test suite Seman [for TigerSeman testing] in progress =======" >>
  putStrLn "Resultados con calculos de escapes" >>
  testerPrintDir "./test/test_code/good" >> 
  putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "Revisar etapas previas al análisis semántico, y código del programa")
                (\s -> (fst $ runSt (runSeman (either (fail $ "Revisar calculo de escapes") 
                                                      id (calcularEEsc s))) 0)) . parse

testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show) 
            (\exp -> putStrLn $ show $ fst $ runSt (runSeman (either (fail "Revisar calculo de escapes")
                                                                     id (calcularEEsc exp))) 0) (parse str)

testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
