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

import State
import TigerAbs
import TigerCanon
import TigerEscap
import TigerFrame
import TigerParser
import TigerPretty
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTips
import TigerTemp
import TigerTrans
import TigerUnique

import Text.Parsec (runParser)

data Options = Options {
        optArbol     :: Bool,
        optDebEscap :: Bool,
        optSeman :: Bool,
        optFrags :: Bool,
        optCanon :: Bool}
    deriving Show

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Opciones de consola ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --
header = "Se usa: tiger fileName [OPTIONS]"

defaultOptions :: Options
defaultOptions = Options {optArbol = False, 
                          optDebEscap = False, 
                          optSeman = False, 
                          optFrags = False,
                          optCanon = False}

options :: [OptDescr (Options -> Options)]
options = [Option "a" ["arbol"] (NoArg (\opts -> opts{optArbol = True})) 
                  "Muestra el AST luego de haber realizado el cálculo de escapes",
           Option "e" ["escapada"] (NoArg (\opts -> opts{optDebEscap = True})) "Stepper escapadas",
           Option "s" ["seman"] (NoArg (\opts -> opts{optSeman = True})) 
                  "Resultado de seman: código intermedio y tipo de la expresión",
           Option "f" ["frags"] (NoArg (\opts -> opts{optFrags = True}))
                  "Fragmentos resultantes de la expresión",
           Option "c" ["canon"] (NoArg (\opts -> opts{optCanon = True})) 
                  "Trabajar con resultados canonizados"]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                         (o, n, [])   -> return (foldl (flip id) defaultOptions o, n)
                         (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

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

tempLabExp :: Exp -> Either Symbol (BExp, Tipo)
tempLabExp ast = fst $ evalState (runSeman ast) 0
  -- something <- canonM sometree :: StGen [Stm]

showSeman :: Either Symbol (BExp, Tipo) -> IO () 
showSeman res =
  either (\err -> error $ "Error in seman: " ++ unpack err)
         (\(bexp, t) -> putStrLn $ renderBIr bexp ++ " :: " ++ show t)
         res

fragsExp :: Exp -> Either Symbol [TransFrag]
fragsExp ast = fst $ evalState (runTransProg ast) 0

showFrags :: Either Symbol [TransFrag] -> IO ()
showFrags res = 
  either (\err -> error $ "Error in frags translation: " ++ unpack err)
         (\f -> mapM_ (\x -> do putStrLn "----------"
                                putStrLn x) (map renderFrag f))
         res

canonExp :: Exp -> IO ()
canonExp exp = 
  do let res = fst $ runSt (runTransProg exp) 0
     either (\err -> error $ "Error in canonization: " ++ unpack err)
            (\f -> mapM_ (\x -> do putStrLn "----------"
                                   putStrLn $ either (\(s, fr) -> renderPCan s fr) (renderFrag) x) 
                         (fst $ runSt (evalStateT (canonize f) firstTank) 0))
            res

main :: IO ()
main = 
  do args <- Env.getArgs
     let isempty = null args
     let s = if isempty then "" else head args
     let opts = if isempty then [] else tail args
     (opts', _) <- compilerOptions opts
     sourceCode <- if isempty then fail header else readFile s
     rawAst <- parserStep opts' s sourceCode
     ast <- calculoEscapadas rawAst opts'
     when (optArbol opts') (putStrLn "" >> showExp ast >> putStrLn "")
     let seman = tempLabExp ast
     when (optSeman opts') (showSeman seman >> putStrLn "") 
     let frags = fragsExp ast
     when (optFrags opts' && (not $ optCanon opts')) (showFrags frags)
     when (optFrags opts' && optCanon opts') (canonExp ast)
