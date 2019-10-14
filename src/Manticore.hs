module Manticore where

import TigerErrores as E
import TigerSres
import TigerSymbol
import TigerTips
import TigerUnique
import TigerTrans

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Map as M

import Debug.Trace (trace)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

type Monada = ExceptT Symbol (StateT Estado StGen)

data Estado = Est {vEnv :: M.Map Symbol EnvEntry, tEnv :: M.Map Symbol Tipo}
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
      do oldEst <- get
         put (oldEst{vEnv = M.insert sym (Var ventry) (vEnv oldEst)})
         a <- w
         put oldEst
         return a
    -- insertFunV :: Symbol -> FunEntry -> w a -> w a
    insertFunV sym fentry w =
      do oldEst <- get
         put (oldEst{vEnv = M.insert sym (Func fentry) (vEnv oldEst)})
         a <- w
         put oldEst
         return a
    -- insertVRO :: Symbol -> ValEntry -> w a -> w a
    insertVRO sym ventry w =
      do oldEst <- get
         put (oldEst{vEnv = M.insert sym (Var ventry) (vEnv oldEst)})
         a <- w
         put oldEst
         return a
    -- insertTipoT :: Symbol -> Tipo -> w a -> w a
    insertTipoT sym t w = 
      do oldEst <- get
         put (oldEst{tEnv = M.insert sym t (tEnv oldEst)})
         a <- w
         put oldEst
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

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Helpers ----------------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- | Definimos algunos helpers
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p ++ "\n")

-- | Patrón de errores...
errorTiposMsg :: (Demon w, Show p)
              => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 = flip addpos p
    $ flip adder (pack $ msg ++ "\n")
    $ errorTipos t1 t2

cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _  = derror $ pack "Tienen distintos campos - TigerSeman.cmpZip1\n"
cmpZip _ []  = derror $ pack "Tienen distintos campos - TigerSeman.cmpZip2\n"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr
