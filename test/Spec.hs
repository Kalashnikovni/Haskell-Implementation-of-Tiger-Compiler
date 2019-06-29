import State
import TigerParser (parse)
import TigerSeman
import Tools

main :: IO ()
main = 
  putStrLn "\n======= Test suite Spec [for TigerSeman testing] in progress =======" >>
  putStrLn "Good:" >>
  testDir good_loc (testGood good_loc tester) >>
  putStrLn "Type:" >>
  testDir type_loc (testGood type_loc tester) >>
  putStrLn "Bad:" >>
  testDir bad_loc (testBad bad_loc tester) >>
  putStrLn "\n======= Test suite FIN ======="

tester = either (fail $ "???") (\s -> fst $ runSt (runSeman s) 0) . parse
