module Escap (testerPrint, main) where

import TigerAbs
import TigerEscap
import TigerParser (parse)
import TigerQQ
import TigerSymbol
import TigerUnique
import Tools

import State

import System.Directory

-- Ejemplo de cómo testear...

-- Hay varios ejemplos acá.

-- Dos ejemplos concretos escritos en el archivo directamente :
-- + |ejemplo1| escrito usando el modulo TigerQQ.
-- + |ejemplo2| escrito directamente escribiendo el AST.

-- Dos ejemplos donde tenemos que leerlos y pasamos la url,
-- como mostrar los mensajes, el tester y el nombre del archivo.
-- + escapa.tig, __debe fallar__ y por ende se pone azul al fallar y rojo al no detectar el fallo.
-- + intro.tig, debe ejecutar normalmente y por ende se pasa rojo y azul dsp.

-- Testea toda la carpeta de Good Loc
-- Testea toda la carpeta de Type Loc
-- y termina.

main :: IO ()
main =
  putStrLn "\n======= Test ESCAPES in progress =======" >>
  testerPrintDir "./test/test_code/good" >>
  putStrLn "\n======= Test ESCAPES FIN ======="

type EstadoTest = StGen

parseStage :: String -> EstadoTest Exp
parseStage str =
  either (error . show)
         return
         (parse str)

escapStage :: Exp -> EstadoTest Exp
escapStage exp = 
  either (error . show)
         return
         (calcularEEsc exp)

testerEscap :: String -> EstadoTest Exp
testerEscap str = 
  do res1 <- parseStage str
     escapStage res1

testerPrint :: String -> String -> (Exp -> String) -> IO ()
testerPrint loc f printer =
  do str <- readFile $ loc ++ '/' : f
     putStrLn $ printer $ fst $ runSt (testerEscap str) 0

testerPrintDir :: String -> IO ()
testerPrintDir loc = 
  do fs <- listDirectory loc
     mapM_ (\f -> putStrLn ("*** " ++ f ++ " ***") >> testerPrint loc f show >> putStrLn "***************") fs

{-
ejemplo1 :: Exp -- La variable a escapa.
ejemplo1 = [expr|
                let
                  var a : int := 1
                  function f1(b : int):= a
                in
                  f1(a)
                end|]

ejemplo2 :: Exp -- La variable b no está definida.
ejemplo2 = LetExp
            [ VarDec (pack "a") NoEscapa Nothing (IntExp 1 (Simple 1 2)) (Simple 1 2)
            -- , VarDec "b" Nothing Nothing (IntExp 2 1) 2
            -- , VarDec "c" Nothing Nothing (IntExp 3 1) 3
            , FunctionDec
                    [ (pack "f1"
                      ,[(pack "a1", NoEscapa , NameTy $ pack "int")]
                      , Just $ pack "int",VarExp (SimpleVar $ pack "b") (Simple 5 5)
                      ,(Simple 5 6))
                    ]
            ]
            (IntExp 42 (Simple 8 1))
            (Simple 1 0)

ejemplo3 :: Exp
ejemplo3 = [expr|
                let
                  type intlist = {hd:int, tl:intlist}
                  var lis : intlist := intlist {hd = 0, tl = nil}
                in
                  lis;0
                end|]-}
