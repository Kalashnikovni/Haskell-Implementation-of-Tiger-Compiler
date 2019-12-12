module MonadsInstances where

import TigerErrores as E
import TigerFrame
import TigerSres
import TigerSymbol
import TigerTips
import TigerUnique
import TigerTemp
import TigerTrans

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.List
import Data.Map as M
import Data.Maybe
import Data.Stack

import Debug.Trace (trace)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

type Monada = ExceptT Symbol (StateT Estado StGen)

data Estado = Est {vEnv :: M.Map Symbol EnvEntry, 
                   tEnv :: M.Map Symbol Tipo, 
                   lvl :: [Level],
                   lvlNum :: Int, 
                   exitLabs :: Stack (Maybe Label),
                   frags :: [Frag]}
    deriving Show
 
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Clase de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

class (Demon w, Monad w, UniqueGenerator w) => Manticore w where
  -- | Inserta una Variable al entorno
  insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
  insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
  insertVRO :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una variable de tipo al entorno
  insertTipoT :: Symbol -> Tipo -> w a -> w a
  -- | Busca una función en el entorno
  getTipoFunV :: Symbol -> w FunEntry
  -- | Busca una variable en el entorno. Ver [1]
  getTipoValV :: Symbol -> w ValEntry
  -- | Busca un tipo en el entorno
  getTipoT :: Symbol -> w Tipo
  -- | Funciones de Debugging!
  showVEnv :: w a -> w a
  showTEnv :: w a -> w a
  ugen :: w Unique
  tiposIguales :: Tipo -> Tipo -> w Bool

instance Demon Monada where
  -- derror :: Symbol -> w a 
  derror      =  throwE . pack . (++ "\n") . unpack 
  -- adder :: w a -> Symbol -> w a
  adder w msg = withExceptT (\e -> addStr (unpack msg) e) w 

instance Manticore Monada where
  --insertValV :: Symbol -> ValEntry -> w a -> w a
  insertValV sym ventry w = 
    do oldSt <- get
       let preVEnv = vEnv oldSt
       let preTEnv = tEnv oldSt
       let preOut = exitLabs oldSt 
       put oldSt{vEnv = M.insert sym (Var ventry) preVEnv}
       a <- w
       newSt <- get
       put newSt{vEnv = preVEnv, tEnv = preTEnv, exitLabs = preOut}
       return a
  -- insertFunV :: Symbol -> FunEntry -> w a -> w a
  insertFunV sym fentry w =
    do oldSt <- get
       let preVEnv = vEnv oldSt
       let preTEnv = tEnv oldSt
       let preOut = exitLabs oldSt 
       put oldSt{vEnv = M.insert sym (Func fentry) preVEnv}
       a <- w
       newSt <- get
       put newSt{vEnv = preVEnv, tEnv = preTEnv, exitLabs = preOut}
       return a
  -- insertVRO :: Symbol -> ValEntry -> w a -> w a
  insertVRO sym ventry w =
    case ventry of
      (TInt RO, _, _) -> insertValV sym ventry w
      _               -> internal $ pack "Se intenta insertar una variable que no es RO" 
  -- insertTipoT :: Symbol -> Tipo -> w a -> w a
  insertTipoT sym t w = 
    do oldSt <- get
       let preVEnv = vEnv oldSt
       let preTEnv = tEnv oldSt
       let preOut = exitLabs oldSt 
       put oldSt{tEnv = M.insert sym t preTEnv}
       a <- w
       newSt <- get
       put newSt{vEnv = preVEnv, tEnv = preTEnv, exitLabs = preOut}
       return a
  -- getTipoFunV :: Symbol -> w FunEntry
  getTipoFunV sym =
    do st <- get
       case M.lookup sym $ vEnv st of
         Just (Func f)  -> return f
         Just (Var _)   -> derror (pack "Corregir compilador (fun)")
         Nothing        -> internal $ TigerSymbol.appends [(pack "Error de Haskell (func), "),
                                                           sym,
                                                           (pack " deberia estar en el entorno")] 
  -- getTipoValV :: Symbol -> w ValEntry
  getTipoValV sym =
    do st <- get
       case M.lookup sym $ vEnv st of
         Just (Var v)  -> return v
         Just (Func _) -> derror (pack "Corregir compilador (val)")
         Nothing       -> internal $ TigerSymbol.appends [(pack "Error de Haskell (val), "),
                                                          sym, 
                                                          (pack " deberia estar en el entorno")]
  -- getTipoT :: Symbol -> w Tipo
  getTipoT sym = 
    do st <- get
       case M.lookup sym $ tEnv st of
         Just t  -> return t
         Nothing -> internal $ TigerSymbol.appends [(pack "Error de Haskell (tipo), "),
                                                    sym,
                                                    (pack " deberia estar en el entorno")]
  -- showVEnv :: w a -> w a
  showVEnv w = 
    do st <- get
       trace (show $ vEnv st) w
  -- showTEnv :: w a -> w a
  showTEnv w = 
    do st <- get
       trace (show $ tEnv st) w
  -- ugen :: w Unique
  ugen = mkUnique
  tiposIguales (RefRecord s) l@(TRecord _ u) = do
    st <- getTipoT s
    case st of
      TRecord _ u1 -> return (u1 == u)
      ls@RefRecord{} -> tiposIguales ls l
      e -> E.internal $ pack $ "No son tipos iguales - TigerSeman.tiposIguales1" ++ (show e ++ show st)
  tiposIguales l@(TRecord _ u) (RefRecord s) = do
    st <- getTipoT s
    case st of
      TRecord _ u1 -> return (u1 == u)
      ls@RefRecord{} -> tiposIguales l ls
      e -> E.internal $ pack $ "No son tipos iguales - TigerSeman.tiposIguales2" ++ (show e ++ show st)
  tiposIguales (RefRecord s) (RefRecord s') = do
    s1 <- getTipoT s
    s2 <- getTipoT s'
    tiposIguales s1 s2
  tiposIguales TNil  (RefRecord _) = return True
  tiposIguales (RefRecord _) TNil = return True
  tiposIguales (RefRecord s) e = E.internal $ pack $ "No son tipos iguales - TigerSeman.tiposIguales3" ++ (show e ++ show s)
  tiposIguales e (RefRecord s) = E.internal $ pack $ "No son tipos iguales - TigerSeman.tiposIguales4" ++ (show e ++ show s)
  tiposIguales a b = return (equivTipo a b)

instance MemM Monada where
  -- upLevel :: w ()
  upLvl = 
    do st <- get
       put st{lvlNum = max (Data.List.length $ lvl st) (lvlNum st + 1)}
       return () 
  -- downLvl :: w ()
  downLvl =
    do st <- get
       let stlvl = lvl st
       put st{lvlNum = min 0 $ lvlNum st - 1}
       return () 
  -- pushSalida :: Maybe Label -> w ()
  pushSalida l = 
    do st <- get
       let stlabs = exitLabs st
       put st{exitLabs = stackPush stlabs l}
       return ()
  -- topSalida :: w (Maybe Label)
  topSalida = 
    do st <- get
       let stlabs = exitLabs st
       return $ maybe Nothing id (stackPeek stlabs)
  -- popSalida :: w ()
  popSalida =
    do st <- get
       let stlabs = exitLabs st
       case stackPop stlabs of
         Nothing -> do put st{exitLabs = stackNew}
                       return ()
         Just l  -> do put st{exitLabs = fst l}
                       return ()  
  -- pushLevel :: Level -> w ()
  pushLevel l = 
    do st <- get
       put st{lvl = l : (lvl st), lvlNum = getNlvl l}
       return ()
  -- popLevel :: w ()
  popLevel = 
    do st <- get
       put st{lvl = tail $ lvl st}
       return ()
  -- topLevel :: w Level
  topLevel = 
    do st <- get
       return $ head $ lvl st
  -- pushFrag :: Frag -> w()
  pushFrag f =
    do st <- get
       put st{frags = f:(frags st)}
       return ()
  -- getFrags :: w [Frag]
  getFrags =
    do st <- get
       return $ reverse $ frags st
