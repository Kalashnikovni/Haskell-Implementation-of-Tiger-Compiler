{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import State
import TigerAbs
import TigerCanon
import TigerEscap
import TigerFrame
import TigerLiveness
import TigerParser
import TigerPretty
import TigerPrettyIr
import TigerSeman
import TigerSymbol
import TigerTips
import TigerTemp
import TigerTrans
import TigerUnique

import Stages 

import Control.Monad
import Control.Monad.State hiding (evalState)

import Data.Either
import Data.List as L
import Data.List.Split as S
import Data.Maybe
import Data.Text.Lazy as Lazy

import Debug.Trace

import Prelude as P

import System.Console.GetOpt
import System.Directory
import qualified System.Environment as Env
import System.Exit
import System.IO
import System.Process

import Text.Parsec (runParser)

data Options = Options {
        optParse :: Bool,
        optDebEscap :: Bool,
        optFrags :: Bool,
        optInstrs :: Bool,
        optLiveness :: Bool,
        optRegAlloc :: Bool}
    deriving Show

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Opciones de consola ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

header = "Se usa: tiger fileName [OPTIONS]"

defaultOptions :: Options
defaultOptions = Options {optParse = False,
                          optDebEscap = False, 
                          optFrags = False,
                          optInstrs = False,
                          optLiveness = False,
                          optRegAlloc = False}

options :: [OptDescr (Options -> Options)]
options = [Option "p" ["parse"] (NoArg (\opts -> opts{optParse = True}))
                  "Muestra el resultado de parsear el archivo",
           Option "e" ["escapada"] (NoArg (\opts -> opts{optDebEscap = True})) 
                  "Muestra el resultado del calculo de escapes",
           Option "f" ["frags"] (NoArg (\opts -> opts{optFrags = True}))
                  "Fragmentos resultantes de la expresión",
           Option "i" ["instrs"] (NoArg (\opts -> opts{optInstrs = True}))
                  "Instrucciones seleccionadas para la expresion",
           Option "l" ["liveness"] (NoArg (\opts -> opts{optLiveness = True}))
                  "Análisis de liveness del codigo generado",
           Option "r" ["regAlloc"] (NoArg (\opts -> opts{optRegAlloc = True}))
                  "Asignación de registros"]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv = case getOpt Permute options argv of
                         (o, n, [])   -> return (P.foldl (flip id) defaultOptions o, n)
                         (_, _, errs) -> ioError (userError (P.concat errs ++ usageInfo header options))

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Etapas de compilación --------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

outDir = "./output"
execDir = "./output/exec"

main :: IO ()
main = 
  do args <- Env.getArgs
     let isempty = P.null args
     let s = if isempty then "" else P.head args
     let opts = if isempty then [] else P.tail args
     (opts', _) <- compilerOptions opts
     sourceCode <- if isempty then fail header else readFile s
     let (parse, st1) = runSt (parseStage sourceCode) 0
     when (optParse opts') (do putStrLn $ renderExp parse
                               putStrLn "**********************************************************") 
     let (escap, st2) = runSt (escapStage parse) st1
     when (optDebEscap opts') (do putStrLn $ renderExp escap
                                  putStrLn "********************************************************")
     let (frags, st3) = runSt (intermediateStage escap) st2
     when (optFrags opts') (mapM_ (putStrLn . renderFrag) frags)
     let (instrs, st4) = runSt (instrSelectStage frags) st3
     when (optInstrs opts') (do mapM_ (putStrLn . renderStrFrag) $ fst instrs
                                mapM_ (\(ins, fr) -> do putStr $ renderInstr ins fr
                                                        putStrLn $ show fr
                                                        putStrLn "") $ snd instrs
                                putStrLn "********************************************************")
     let ((fg, vs), st5) = runSt (flowGraph instrs) st4
     let (live, st6) = runSt (livenessStage (fg, vs)) st5
     when (optLiveness opts') (do printMap live
                                  putStrLn "********************************************************"
                                  putStrLn $ "Flow graph en formato .dot escrito en " ++ outDir
                                  createDirectoryIfMissing False outDir
                                  writeFile (outDir ++ "/flowgraph.dot") 
                                            (Lazy.unpack $ defaultVis fg)
                                  putStrLn "********************************************************")
     let (assemInstrs, st7) = runSt (regAllocStage st6 instrs) st6
     when (optRegAlloc opts') (do mapM_ (putStrLn . renderStrFrag) $ fst assemInstrs
                                  mapM_ (\(ins, fr, alloc) -> do putStr $ renderInstr ins fr
                                                                 putStrLn ""
                                                                 putStrLn $ renderFrame fr
                                                                 putStrLn ""
                                                                 putStrLn $ show alloc
                                                                 putStrLn "") (snd assemInstrs)) 
     let (newFileName, _) = P.span (\c -> c /= '.') (L.last $ S.splitOn "/" s)
     let assemName = outDir ++ "/" ++ newFileName ++ ".s"
     writeFile assemName 
               (".data\n" ++
                (P.concat $ P.map renderStrFrag (fst assemInstrs)) ++ 
                "\n.text\n" ++
                ".globl tigermain\n" ++
                (P.concat $ P.map (\(ins, fr, _) -> renderInstr ins fr) (snd assemInstrs)) ++
                "\nfinal:\n" ++
                (P.concat $ P.map formatInstr (mkEpil defaultFrame)))
     callCommand $ "gcc -g " ++ assemName ++ " " ++ outDir ++ "/runtime.c" ++ 
                   " -o " ++ execDir ++ "/" ++ newFileName 








