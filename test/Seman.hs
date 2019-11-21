import State
import TigerParser (parse)
import TigerSeman
import TigerSymbol
import Tools

--main :: IO ()
--main = putStrLn "\n====== Test suite DEFAULT ======"

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
