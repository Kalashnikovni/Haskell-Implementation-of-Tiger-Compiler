import State
import TigerParser (parse)
import TigerSeman
import TigerSymbol
import Tools

main :: IO ()
main = 
  putStrLn "\n======= Test suite Translate [for TigerTrans testing] in progress =======" >>
  putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "Revisar etapas previas al análisis semántico, y código del programa")
                (\s -> (fst $ runSt (runTransProg s) 0)) . parse

-- Lo dejamos por las dudas si queremos chequear algo más
--showParse = either (fail $ "???") (\exp -> fail $ show exp) . parse
