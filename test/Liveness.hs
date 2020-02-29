import State
import TigerAssem
import TigerCanon
import TigerEscap
import TigerFrame hiding (exp)
import TigerLiveness
import TigerMakeGraph
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
  putStrLn "\n======= Test suite Liveness [] in progress =======" >>
  --testerPrintDir "./test/test_code/good" testerLiveness >>
  putStrLn "\n======= Test suite FIN ======="  

applyCodeGen :: (Assembler w) => (Stm, Frame) -> w ([Instr], Frame)
applyCodeGen (s, f) = do res <- codeGen s
                         return (res, f)

runInstrSelect = runMonada3 . applyCodeGen
{-
testerLiveness loc f =
  do str <- readFile $ loc ++ '/' : f
     either (putStrLn . show)
            (\exp -> let (res, st) = runSt (runTransProg $ escapTest exp) 0
                     in either pError 
                               (\tfs -> either pError
                                               (\l -> mapM_ (\(instrs, _) -> pGraph instrs) l) 
                                               (mapM (\tf -> fst $ runSt (runInstrSelect tf) st) (snd $ sepFrag tfs)))
                               res) 
            (parse str)
  where pError = putStrLn . show
        pGraph instrs = let (fg, vs) = i2gWithJumps instrs $ instrs2graph instrs
                        in do putStrLn $ show fg
                              putStrLn $ show $ interferenceGraph fg
        escapTest e = either (fail "Revisar calculo de escapes")
                             id (calcularEEsc e)

testerPrintDir loc tp = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> tp loc f >> putStrLn "***************") fs
-}
