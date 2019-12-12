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
  --testDir good_loc showParse >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "Revisar etapas previas al análisis semántico, y código del programa")
                (\s -> (fst $ runSt (runTransProg s) 0)) . parse

-- Lo dejamos por las dudas si queremos chequear algo más
showParse = either (\err -> print $ show err) (\exp -> print $ show exp) . parse
