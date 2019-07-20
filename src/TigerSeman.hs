{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TigerSeman where

import TigerAbs
import TigerErrores as E
import TigerSres
import TigerSymbol
import TigerTips
import TigerUnique
import TigerTopSort

import TigerTemp
-- import TigerTrans

-- Monads
import qualified Control.Conditional as C
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

-- Data
import Data.List as List
import Data.Map as M
import Data.Ord as Ord

import Prelude as P

import Debug.Trace (trace)

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

depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts

tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil TNil EqOp  = False
tiposComparables TUnit _ EqOp    = False
tiposComparables _ _ EqOp        = True
tiposComparables TNil TNil NeqOp = False
tiposComparables TUnit _ NeqOp   = False
tiposComparables _ _ NeqOp       = True
tiposComparables _ _ _           = True

cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _ = derror $ pack "Tienen distintos campos - TigerSeman.cmpZip1\n"
cmpZip _ [] = derror $ pack "Tienen distintos campos - TigerSeman.cmpZip2\n"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr

splitWith :: (a -> Either b c) -> [a] -> ([b], [c])
splitWith f = P.foldr (\x rs -> either (addIzq rs) (addDer rs) (f x)) ([] , [])

addIzq :: ([a], [b]) -> a -> ([a],[b])
addIzq (as,bs) a = (a : as, bs)

addDer :: ([a], [b]) -> b -> ([a],[b])
addDer (as,bs) b = (as, b : bs)

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de variables ------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

transVar :: (Manticore w) => Var -> w ((), Tipo)
transVar (SimpleVar s)      = 
  do t <- getTipoValV s
     return ((), t)
transVar (FieldVar v s)     =
  do (_, t) <- transVar v
     case t of
       TRecord l _ -> maybe (E.internal $ pack "Se intenta acceder a un campo que no pertenece al record") 
                            (\tx -> return ((), tx))
                            (buscarM s l) 
       _           -> E.internal $ pack "Se intenta acceder al campo de una variable que no es un record"
transVar (SubscriptVar v e) =
  do (_, te) <- transExp e 
     case te of
       TInt _ -> do (_, tv) <- transVar v
                    case tv of
                      TArray ta _ -> return ((), ta)
                      _           -> E.internal $ pack "Se intenta indexar algo que no es un arreglo"
       _      -> E.internal $ pack "El indice del arreglo no es un número"

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de tipos ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = getTipoT s
transTy (RecordTy flds) =
  do u   <- ugen
     res <- foldM (\((TRecord lf u'), p) (s, t) -> do t' <- transTy t
                                                      return (TRecord ((s, t', p):lf) u', p + 1)) ((TRecord [] u), 0) flds'
     return $ fst res
  where flds' = List.sortBy (Ord.comparing fst) flds
transTy (ArrayTy s)     =
  do ts <- getTipoT s
     u <- ugen
     return $ TArray ts u

fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _          = P.error "No deberia haber una definición de tipos en los args..."

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de declaraciones --------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a
transDecs :: Manticore w => [Dec] -> w a -> w a
transDecs [] w                               = w
transDecs ((VarDec nm escap t init p): xs) w = 
  do (_, ti) <- transExp init
     case t of
       Just sv -> do tv <- getTipoT sv
                     unless (equivTipo tv ti) $ errorTiposMsg p "El tipo del valor inicial es incorrecto" tv ti
       Nothing -> unless (not $ equivTipo ti TNil) $ errorTiposMsg p "Debe usar la forma extendida" ti TNil
     insertValV nm ti $ transDecs xs w
transDecs ((FunctionDec fs) : xs)          w =
  let 
    env = insertFFold fs w 
  in    
    do mapM_ (\f@(_, params, tf, bd, p) -> 
               do --insertFFFold params env 
                  (_, t) <- insertFFold fs $ insertFFFold params $ transExp bd
                  case tf of
                    Just td -> do tdd <- getTipoT td
                                  unless (equivTipo t tdd) $
                                         errorTiposMsg p "El valor retornado no es del tipo declarado" t tdd
                    Nothing -> unless (equivTipo t TUnit) $ errorTiposMsg p "La funcion devuelve un valor" t TUnit) fs 
       transDecs xs env
transDecs ((TypeDec xs) : xss)             w =
  let 
    xs'     = fmap (\(x,y,_) -> (x,y)) xs
    tyNames =  fst $ unzip xs'
    (recordsTy, nrTy) = splitWith (\(s , t) -> either (Left . (s,)) (Right . (s,)) (splitRecordTy t)) xs'
    sortedTys = kahnSort nrTy
  in 
    insertRecordsAsRef recordsTy $
    insertSortedTys sortedTys $
    selfRefs recordsTy xs' $ 
    transDecs xss w

insertFFold :: Manticore w => [(Symbol, [(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)] -> w a -> w a
insertFFold [] w                              = w
insertFFold ((nm, params, res, bd, _) : fs) w =
  do u  <- ugen 
     ts <- mapM (\(_, _, p) -> transTy p) params
     case res of
       Just t  -> do tt <- getTipoT t
                     insertFunV nm (u, nm, ts, tt, Propia) $ insertFFold fs w
       Nothing -> insertFunV nm (u, nm, ts, TUnit, Propia) $ insertFFold fs w

insertFFFold :: Manticore w => [(Symbol, Escapa, Ty)] -> w a -> w a
insertFFFold [] w                    = w
insertFFFold ((nm, _, t) : params) w =
  do tt <- transTy t
     insertValV nm tt $ insertFFFold params w

insertRecordsAsRef  :: Manticore w => [(Symbol, Ty)] -> w a -> w a
insertRecordsAsRef [] m                 = m
insertRecordsAsRef ((rSym, rTy) : rs) m =
  insertTipoT rSym (RefRecord rSym) $ insertRecordsAsRef rs m

insertSortedTys :: Manticore w => [(Symbol, Ty)] -> w a -> w a
insertSortedTys [] m = m
insertSortedTys ((tSym, tTy) : ts) m =
  do tty <- transTy tTy
     insertTipoT tSym tty $ insertSortedTys ts m

-- Primer argumento: lista de records
-- Segundo argumento: lista de todos los tipos
selfRefs :: Manticore w => [(Symbol, Ty)] -> [(Symbol, Ty)] -> w a -> w a
selfRefs [] ls w            = w
selfRefs ((sr, tr):rs) ls w =
  do tr' <- transTy tr
     case tr' of 
       TRecord rs' u -> insertTipoT sr tt $ updateRest (sr, tt) ls $ selfRefs rs ls w  
         where tt = TRecord (P.map (\(s, ti, p) -> (s, autoRef sr tt ti, p)) rs') u 
       _             -> internal $ pack "Error de Haskell, recordTy no tiene tipo TRecord"

updateRest :: Manticore w => (Symbol, Tipo) -> [(Symbol, Ty)] -> w a -> w a
updateRest _ [] w                  = w
updateRest (s, ts) ((sr, tr):rs) w = 
  updateRefs s ts sr $ updateRest (s, ts) rs w


autoRef :: Symbol -> Tipo -> Tipo -> Tipo
autoRef s t r@(RefRecord s') 
  | s == s'   = t 
  | otherwise = r
autoRef _ _ r = r

updateRefs :: Manticore w => Symbol -> Tipo -> Symbol -> w a -> w a
updateRefs  s t s' m = 
  do t'  <- getTipoT s'
     case t' of
       TArray ta u  -> 
         insertTipoT s' (TArray (autoRef s t ta) u) m 
       TRecord ls u -> 
         insertTipoT s' (TRecord (P.map (\(ss, tt, pp) -> (ss, autoRef s t tt, pp)) ls) u) m
       _            -> 
         insertTipoT s' (autoRef s t t') m

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de expresiones ----------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p) = 
  addpos (transVar v) p
transExp UnitExp{} = 
  return ((), TUnit) -- ** fmap (,TUnit) unitExp
transExp NilExp{} = 
  return ((), TNil) -- ** fmap (,TNil) nilExp
transExp (IntExp i _) = 
  return ((), TInt RW) -- ** fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = 
  return (() , TString) -- ** fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) =
  do (_, _, tfargs, tf, _) <- getTipoFunV nm
     targs <- mapM transExp args
     zipWithM_ (\ta tb -> do unless (equivTipo ta (snd tb)) $ 
                                     errorTiposMsg p "No coincide el tipo de los argumentos" ta (snd tb)) tfargs targs 
     return ((), tf)
transExp (OpExp el' oper er' p) = 
  do (_ , el) <- transExp el'
     (_ , er) <- transExp er'
     case oper of
       EqOp  -> if tiposComparables el er EqOp then oOps el er
                else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
       NeqOp -> if tiposComparables el er EqOp then oOps el er
                else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
       PlusOp -> oOps el er
       MinusOp -> oOps el er
       TimesOp -> oOps el er
       DivideOp -> oOps el er
       LtOp -> oOps el er
       LeOp -> oOps el er
       GtOp -> oOps el er
       GeOp -> oOps el er
  where oOps l r = if equivTipo l r 
                      && equivTipo l (TInt RO) 
                   then return ((), TInt RO)
                   else addpos (derror (pack "Error en el chequeo de una comparacion.")) p
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \x -> case x of 
    trec@(TRecord fldsTy _) -> 
      do fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds 
         let ordered = List.sortBy (Ord.comparing fst) fldsTys
             ordered' = List.sortBy (Ord.comparing fst3) fldsTy
         _ <- flip addpos p $ cmpZip ((\(s,(c,t)) -> (s,t)) <$> ordered) ordered' 
         return ((), trec) 
    t -> errorTiposMsg p "La variable no es de tipo record" t (TRecord [] 0)
  where fst3 (a, _, _) = a
transExp(SeqExp es p) = fmap last (mapM transExp es)
transExp(AssignExp var val p) =
  do (_, tvar) <- transVar var
     (_, tval) <- transExp val
     case equivTipo tvar tval of
       True -> return ((), TUnit) 
       _    -> errorTiposMsg p "El tipo de la variable y del valor no son iguales" tvar tval  
transExp(IfExp co th Nothing p) = do
  (_ , co') <- transExp co
  unless (equivTipo co' TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" co' TBool 
  (() , th') <- transExp th
  unless (equivTipo th' TUnit) $ errorTiposMsg p "La branch está devolviendo un resultado" th' TUnit
  return (() , TUnit)
transExp(IfExp co th (Just el) p) = do
  (_ , condType) <- transExp co
  unless (equivTipo condType TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" condType TBool
  (_, ttType) <- transExp th
  (_, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "Las branches devuelven resultados de distinto tipo" ttType ffType
  return ((), ttType)
transExp(WhileExp co body p) = do
  (_ , coTy) <- transExp co
  unless (equivTipo coTy TBool) $ errorTiposMsg p "La condicion del While no es booleana" coTy TBool
  (_ , boTy) <- transExp body
  unless (equivTipo boTy TUnit) $ errorTiposMsg p "El cuerpo del While devuelve un resultado" boTy TBool
  return ((), TUnit)
-- TODO: ¿Cómo chequeamos que nv sea una variable "fresca"?
-- TODO: ¿Acá tenemos que chequear si lo < hi?
transExp(ForExp nv mb lo hi bo p) =
  do (_, tlo) <- transExp  lo
     unless (equivTipo tlo (TInt RW)) $ errorTiposMsg p "La cota inferior del for no es un entero modificable" tlo (TInt RW)
     (_, thi) <- transExp  hi
     unless (equivTipo thi (TInt RW)) $ errorTiposMsg p "La cota superior del for no es un entero modificable" thi (TInt RW)
     (_, tbo) <- insertValV nv (TInt RO) $ transExp bo
     unless (equivTipo tbo TUnit) $ errorTiposMsg p "El cuerpo del for está devolviendo un valor" tbo TUnit
     return ((), TUnit)
transExp(LetExp dcs body p) = transDecs dcs $ transExp body
transExp(BreakExp p) = return ((), TUnit)
transExp(ArrayExp sn cant init p) =
  do tsn <- getTipoT sn 
     -- TODO: corregir bien los campos de TArray, completamos con valores por defecto para que funcione.
     case tsn of
       TArray ta _ -> do (_, tca) <- transExp cant
                         unless (equivTipo tca (TInt RO)) $ errorTiposMsg p "El indice no es un entero" tca (TInt RO)
                         (_, tin) <- transExp init
                         unless (equivTipo ta tin) $ errorTiposMsg p "El valor inicial no es del tipo del arreglo" ta tin
                         return ((), tsn)
       _           -> errorTiposMsg p "La variable no es de tipo arreglo" tsn (TArray TUnit 0)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Clase de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

class (Demon w, Monad w, UniqueGenerator w) => Manticore w where
  -- | Inserta una Variable al entorno
    insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
    insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
    insertVRO :: Symbol -> w a -> w a
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

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Instancia de estados ---------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data Estado = Est {vEnv :: M.Map Symbol EnvEntry, tEnv :: M.Map Symbol Tipo}
    deriving Show

initConf :: Estado
initConf = Est
           { tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
           , vEnv = M.fromList
                    [(pack "print", Func (1,pack "print",[TString], TUnit, Runtime))
                    ,(pack "flush", Func (1,pack "flush",[],TUnit, Runtime))
                    ,(pack "getchar",Func (1,pack "getchar",[],TString,Runtime))
                    ,(pack "ord",Func (1,pack "ord",[TString],TInt RW,Runtime))
                    ,(pack "chr",Func (1,pack "chr",[TInt RW],TString,Runtime))
                    ,(pack "size",Func (1,pack "size",[TString],TInt RW,Runtime))
                    ,(pack "substring",Func (1,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime))
                    ,(pack "concat",Func (1,pack "concat",[TString,TString],TString,Runtime))
                    ,(pack "not",Func (1,pack "not",[TBool],TBool,Runtime))
                    ,(pack "exit",Func (1,pack "exit",[TInt RW],TUnit,Runtime))]}

type Monada = ExceptT Symbol (StateT Estado StGen)

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
    -- insertVRO :: Symbol -> w a -> w a
    insertVRO sym w =
      do oldEst <- get
         put (oldEst{vEnv = M.insert sym (Var $ TInt RO) (vEnv oldEst)})
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

runMonada :: Monada ((), Tipo) -> StGen (Either Symbol ((), Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol ((), Tipo))
runSeman = runMonada . transExp
