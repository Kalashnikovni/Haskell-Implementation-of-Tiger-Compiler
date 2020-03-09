import TigerEscap
import TigerSeman
import TigerSymbol
import Tools

import State
import System.Directory
import TigerParser (parse)

main :: IO ()
main = 
  putStrLn "\n======= Test suite Translate [for TigerTrans testing] in progress =======" >>
  putStrLn "Show results good:" >>
  testerPrintDir "./test/test_code/good" >>
  {-putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>-}
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "Revisar etapas previas al análisis semántico, y código del programa")
                (\s -> (fst $ runSt (runTransProg (either (fail "Revisar calculo de escapes")
                                                          id (calcularEEsc s))) 0)) . parse

testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show) 
            (\exp -> putStrLn $ show $ fst $ runSt 
                       (runTransProg (either (fail "Revisar calculo de escapes") 
                                             id (calcularEEsc exp))) 0) (parse str)

testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs

-- Lo dejamos por las dudas si queremos chequear algo más
showParse = either (\err -> print $ show err) (\exp -> print $ show exp) . parse
