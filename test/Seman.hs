import State
import TigerParser (parse)
import TigerSeman
import TigerSymbol
import Tools

-- *** Test suite pensada para evaluar la primera etapa del compilador.
-- *** Es decir, principalmente el chequeo de tipos de expresiones.
-- *** En la segunda etapa metemos mano de lo que hicimos en la primera
-- *** etapa, así que esta test suite pierde un poco el sentido.

main :: IO ()
main = 
  putStrLn "\n======= Test suite Seman [for TigerSeman testing] in progress =======" >>
  putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "Revisar etapas previas al análisis semántico, y código del programa")
                (\s -> (fst $ runSt (runSeman s) 0)) . parse
