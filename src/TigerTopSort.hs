{-# Language OverloadedStrings #-}

module TigerTopSort (kahnSort) where

import Manticore
import TigerAbs
import TigerErrores
import TigerSymbol (Symbol)

import Control.Monad.Trans.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Debug.Trace

type DepMap = M.Map Symbol [Symbol]

data GraphRet = GR {deps :: DepMap
                    , ret  :: [Symbol]}

data ErrCiclo = Ciclo

addT :: Symbol -> ExceptT ErrCiclo (State GraphRet) ()
addT x = modify (\st -> st {ret = x : ret st})

seen :: Symbol -> DepMap -> DepMap
seen s = M.insertWith (++) s []

buildDepMap :: [(Symbol, Ty)] -> DepMap
buildDepMap [] = M.empty
buildDepMap ((sTy, NameTy s) : xs) =
  seen s $ M.insertWith (++) sTy [s] (buildDepMap xs)
buildDepMap ((sTy, RecordTy ss) : xs) =
  buildDepMap (zip (repeat sTy) (fmap snd ss) ++ xs)
buildDepMap ((sTy, ArrayTy s) : xs) =
  seen s $ M.insertWith (++) sTy [s] (buildDepMap xs)

removeSym :: Symbol -> DepMap -> DepMap
removeSym s =
  M.delete s . M.map (delete s)

checkIncoming :: Symbol -> DepMap -> Bool
checkIncoming s = M.foldl (\b ss -> b || elem s ss) False

noEdges :: DepMap -> Bool
noEdges = M.foldl (\b rs -> b && null rs) True

iterador :: [Symbol] -> ExceptT ErrCiclo (State GraphRet) ()
iterador [] = 
  do depMap <- gets deps
     if noEdges depMap 
        then return () 
        else throwE Ciclo 
iterador (s : ss) = 
  do addT s 
     ds <- gets (flip (M.!) s . deps) 
     modify (\st -> st {deps = M.delete s (deps st)}) 
     dps <- gets deps 
     let s' = filter (not . flip checkIncoming dps) ds
     iterador (s' ++ ss) 

kahnSort' :: Manticore w => [(Symbol, Ty)] -> w [Symbol]
kahnSort' xs = 
  either (\e -> derror "Hay ciclo en las declaraciones de tipo") (\v -> return $ ret fs) res
  where initialDeps = buildDepMap xs
        initialSyms = filter (not . flip checkIncoming initialDeps) $ map fst xs
        (res, fs)   = runState (runExceptT $ iterador initialSyms) (GR initialDeps [])

kahnSort :: Manticore w => [(Symbol, Ty)] -> w [(Symbol, Ty)]
kahnSort xs = 
  do xs' <- kahnSort' xs 
     return $ concat $ map (\x -> filter (\y -> x == fst y) xs) xs'
