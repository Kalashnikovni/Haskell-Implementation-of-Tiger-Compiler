{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where
import Control.Monad
import Control.Monad.State hiding (evalState)
import Data.Either
import Data.Maybe
import System.Console.GetOpt
import qualified System.Environment as Env
import System.Exit

import TigerAbs
import TigerEscap
import TigerParser
import TigerPretty
import TigerSeman
import TigerTemp
import TigerUnique

import Text.Parsec (runParser)

data Options = Options {
        optArbol     :: Bool,
        optDebEscap :: Bool}
    deriving Show

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Opciones de consola ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

defaultOptions :: Options
defaultOptions = Options {optArbol = False, optDebEscap = False}

options :: [OptDescr (Options -> Options)]
options = [Option ['a'] ["arbol"] (NoArg (\opts -> opts {optArbol = True})) 
                  "Muestra el AST luego de haber realizado el cálculo de escapes",
           Option ['e'] ["escapada"] (NoArg (\opts -> opts {optDebEscap = True})) "Stepper escapadas"]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                         (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
                         (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Se usa: tiger fileName [OPTIONS]"

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Etapas de compilación --------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

showExp :: Exp -> IO ()
showExp e = 
  do putStrLn "Mostramos el AST (PP Gracias a Emilio Lopez Junior)"
     putStrLn $ renderExp e

parserStep :: Options -> String -> String -> IO Exp
parserStep opts nm sc = 
  either (\perr -> error $ "Parser error" ++ show perr)
         return
         $ runParser expression () nm sc


calculoEscapadas :: Exp -> Options -> IO Exp
calculoEscapadas rawAST opts =
  if (optDebEscap opts)
  then
    fail "NO DEBBUGING!"
  else
    either (\err ->
               putStrLn "Error de Escap:" >>
               fail (show err)) 
           return (calcularEEsc rawAST)

tempLabExp :: Exp -> StGen ()
tempLabExp ast = 
  do treeS <- runSeman ast
     -- something <- canonM sometree :: StGen [Stm]
     return ()


main :: IO ()
main = 
  do args <- Env.getArgs
     let s = head args
     let opts = tail args
     (opts', _) <- compilerOptions opts
     sourceCode <- readFile s
     rawAst <- parserStep opts' s sourceCode
     ast <- calculoEscapadas rawAst opts'
     when (optArbol opts') (showExp ast)
     let _ = evalState (tempLabExp ast) 0
     print "Genial!"
