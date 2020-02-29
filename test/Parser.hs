import Text.Parsec
import TigerParser
import TigerPretty
import Tools
import System.Directory

main :: IO ()
main =
  putStrLn "\n======= Test PARSER in progress =======" >>
  putStrLn "escapa.tig" >>
  (test "./test/test_code" (badRes . show) (const $ bluenice) tester "escapa.tig") >>
  putStrLn "intro.tig" >>
  (test "./test/test_code" (badRes . show) (const $ bluenice) tester "intro.tig") >>
  putStrLn "Print results, good:" >>
  testerPrintDir "./test/test_code/good" >>
  putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test FIN ======="

tester s = runParser expression () s s

-- Si queremos ver el resultado del parseo
-- Sencillamente lo bindeamos en el main de este archivo:
-- ... >>
-- testerPrint "location" >>
-- ...
testerPrint loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show) (\exp -> putStrLn $ show exp) $ runParser expression () str str

-- Ahora para testear printeando un directorio entero
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f >> putStrLn "***************") fs
