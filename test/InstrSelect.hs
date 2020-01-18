import State
import TigerAssem
import TigerCanon
import TigerEscap
import TigerFrame hiding (exp)
import TigerMunch
import TigerParser (parse)
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTrans
import TigerTree
import TigerUnique
import Tools

import System.Directory 

main :: IO ()
main = 
  putStrLn "\n======= Test suite Instruction Selection [] in progress =======" >>
  testerPrintDir "./test/test_code/good" testerPrintInstr >>
  putStrLn "Test Canon results" >>
  testerPrintDir "./test/test_code/good" testerPrintCanon >>
  putStrLn "\n======= Test suite FIN ======="  

applyCodeGen :: (Assembler w) => (Stm, Frame) -> w ([Instr], Frame)
applyCodeGen (s, f) = do res <- codeGen s
                         return (res, f)

applyCanonTest :: (Assembler w) => [TransFrag] -> w [([Stm], Frame)]
applyCanonTest tf = 
  mapM (\(s, f) -> do s' <- canonM s
                      return (s', f)) (snd $ sepFrag tf)

runInstrSelect = runMonada3 . applyCodeGen
runCanonTest = runMonada4 . applyCanonTest

testerPrintCanon loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show)
            (\exp -> either (putStrLn . show)
                            (\tfs -> either (putStrLn . show)
                                            (\l -> mapM_ (\(stms, fr) -> putStrLn $ show stms) l)
                                            (fst $ runSt (runCanonTest tfs) 0))
                            (fst $ runSt (runTransProg exp) 0))
            (parse str)

testerPrintInstr loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show)
            (\exp -> let (res, st) = runSt (runTransProg $ escapTest exp) 0
                     in either pError 
                               (\tfs -> either pError
                                               (\l -> mapM_ (\(instrs, fr) -> pInstrs instrs fr) l) 
                                               (mapM (\tf -> fst $ runSt (runInstrSelect tf) st) (snd $ sepFrag tfs)))
                               res) 
            (parse str)
  where pError = putStrLn . show
        pInstrs ins fram = let ff = procEntryExit3 fram $ procEntryExit2 fram ins
                           in putStrLn $ prolog ff ++ concat (map (format opmakestring) (body ff)) ++ epilogue ff
        escapTest e = either (fail "Revisar calculo de escapes")
                             id (calcularEEsc e)

testerPrintDir loc tp = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> tp loc f >> putStrLn "***************") fs
