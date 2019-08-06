{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TigerSeman where

import Manticore
import TigerAbs
import TigerErrores as E
import TigerSres
import TigerSymbol
import TigerTips
import TigerTopSort
import TigerUnique

import TigerTemp
-- import TigerTrans

-- Monads
import qualified Control.Conditional as C
import Control.Monad (foldM, zipWithM_)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except

-- Data
import Data.List as List
import Data.Map as M
import Data.Maybe
import Data.Ord as Ord

import Prelude as P


-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Helpers ----------------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- addpos :: (Demon w, Show b) => w a -> b -> w a
-- errorTiposMsg :: (Demon w, Show p) => p -> String -> Tipo -> Tipo -> w a
-- cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool

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
       TRecord l _ -> maybe (derror $ pack "Se intenta acceder a un campo que no pertenece al record") 
                            (\tx -> return ((), tx))
                            (buscarM s l) 
       _           -> derror $ pack "Se intenta acceder al campo de una variable que no es un record"
transVar (SubscriptVar v e) =
  do (_, te) <- transExp e 
     case te of
       TInt _ -> do (_, tv) <- transVar v
                    case tv of
                      TArray ta _ -> return ((), ta)
                      _           -> derror $ pack "Se intenta indexar algo que no es un arreglo"
       _      -> derror $ pack "El indice del arreglo no es un número"

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
                     C.unless (equivTipo tv ti) $ errorTiposMsg p "El tipo del valor inicial es incorrecto" tv ti
                     insertValV nm tv $ transDecs xs w
       Nothing -> do C.when (ti == TNil) $ errorTiposMsg p "Debe usar la forma extendida" ti TNil
                     insertValV nm ti $ transDecs xs w
transDecs ((FunctionDec fs) : xs)          w =
  do mapM_ (\f@(_, params, tf, bd, p) -> 
             do (_, t) <- insertFFold fs $ insertFFFold params $ transExp bd
                case tf of
                  Just td -> do tdd <- getTipoT td
                                C.unless (equivTipo t tdd) $
                                         errorTiposMsg p "El valor retornado no es del tipo declarado" t tdd
                  Nothing -> C.unless (equivTipo t TUnit) $ errorTiposMsg p "La funcion devuelve un valor" t TUnit) fs 
     insertFFold fs $ transDecs xs w
transDecs ((TypeDec xs) : xss)             w =
  let 
    checkNames [] w'     = w' 
    checkNames (z:zs) w' = 
      case elem (fst z) (P.map fst zs) of
        False -> checkNames zs w'
        True  -> addpos (derror $ pack "Hay dos tipos con el mismo nombre en un batch") (snd z) 
    xs'     = fmap (\(x,y,_) -> (x,y)) xs
    tyNames =  fst $ unzip xs'
    (recordsTy, nrTy) = splitWith (\(s , t) -> either (Left . (s,)) (Right . (s,)) (splitRecordTy t)) xs'
  in 
    checkNames (P.map (\(a, _, c) -> (a, c)) xs) $
    do sortedTys <- kahnSort nrTy
       (insertRecordsAsRef recordsTy $
        insertSortedTys sortedTys $
        selfRefs recordsTy xs' $ 
        transDecs xss w)

insertFFold :: Manticore w => [(Symbol, [(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)] -> w a -> w a
insertFFold [] w                              = w
insertFFold ((nm, params, res, bd, p) : fs) w =
  do u  <- ugen 
     ts <- mapM (\(_, _, p) -> transTy p) params
     case elem nm $ P.map fst5 fs of
       False ->
         case res of
           Just t  -> do tt <- getTipoT t
                         insertFunV nm (u, nm, ts, tt, Propia) $ insertFFold fs w
           Nothing -> insertFunV nm (u, nm, ts, TUnit, Propia) $ insertFFold fs w
       True -> addpos (derror $ pack "Hay dos funciones con el mismo nombre en un batch") p 
  where fst5 (a, _, _, _, _) = a

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
       _             -> E.internal $ pack "Error de Haskell, recordTy no tiene tipo TRecord"

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
     mapM_ checkBreaks args
     targs <- mapM transExp args
     C.when (P.length tfargs /= P.length targs) $ addpos (derror $ pack ("La cantidad de argumentos pasados no coincide " ++
                                                                       "con la cantidad de argumentos de la declaracion")) p
     zipWithM_ (\ta tb -> do C.unless (equivTipo ta (snd tb)) $ 
                                       errorTiposMsg p "No coincide el tipo de los argumentos" ta (snd tb)) tfargs targs 
     return ((), tf)
transExp (OpExp el' oper er' p) = 
  do (_ , el) <- transExp el'
     (_ , er) <- transExp er'
     checkBreaks el'
     checkBreaks er'
     case oper of
       EqOp  -> if tiposComparables el er EqOp then eqOps el er
                else errmsg "Error de tipos. Tipos no comparables 1:" el er
       NeqOp -> if tiposComparables el er EqOp then eqOps el er
                else errmsg "Error de tipos. Tipos no comparables 2:" el er
       PlusOp -> oOps el er
       MinusOp -> oOps el er
       TimesOp -> oOps el er
       DivideOp -> oOps el er
       LtOp -> ineqOps el er
       LeOp -> ineqOps el er
       GtOp -> ineqOps el er
       GeOp -> ineqOps el er
  where errmsg msg t1 t2 = addpos (derror $ pack $ msg ++ " " ++ show t1 ++ " " ++ show t2) p
        getUnique (TArray _ u)  = return u
        getUnique (TRecord _ u) = return u
        getUnique _             = addpos (derror $ pack "Error en el chequeo de una comparacion 1.") p
        oOps l r  = if equivTipo l r 
                       && equivTipo l (TInt RO) 
                    then return ((), TInt RO)
                    else errmsg "Error en el chequeo de una comparacion 2." l r
        eqOps TNil TNil = errmsg "Error en el chequeo de una comparacion 3." TNil TNil -- Redundant
        eqOps TNil r    = if equivTipo TNil r
                          then return ((), TInt RO)
                          else errmsg "Error en el chequeo de una comparacion 4." TNil r 
        eqOps l TNil    = if equivTipo l TNil
                          then return ((), TInt RO)
                          else errmsg "Error en el chequeo de una comparacion 5." l TNil 
        eqOps l r = if equivTipo l r &&
                       (equivTipo l (TInt RO) || equivTipo l TString)
                    then return ((), TInt RO)
                    else do l' <- getUnique l
                            r' <- getUnique r 
                            if l' == r'
                            then return ((), TInt RO)
                            else errmsg "No se pueden comparar los tipos 1:" l r
        ineqOps l r = if equivTipo l r &&
                         (equivTipo l (TInt RO) || equivTipo l TString)
                      then return ((), TInt RO)
                      else errmsg "No se pueden comparar los tipos 2:" l r
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \x -> case x of 
    trec@(TRecord fldsTy _) -> 
      do mapM_ checkBreaks (P.map snd flds)
         fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds 
         let ordered  = List.sortBy (Ord.comparing fst) fldsTys
             ordered' = List.sortBy (Ord.comparing fst3) fldsTy
         _ <- flip addpos p $ cmpZip ((\(s,(c,t)) -> (s,t)) <$> ordered) ordered' 
         return ((), trec) 
    t -> errorTiposMsg p "La variable no es de tipo record" t (TRecord [] 0)
  where fst3 (a, _, _) = a
transExp(SeqExp es p) = 
  do mapM_ checkBreaks es
     fmap last (mapM transExp es)
transExp(AssignExp var val p) =
  do checkBreaks val
     (_, tvar) <- transVar var
     (_, tval) <- transExp val
     case equivTipo tvar tval of
       True -> return ((), TUnit) 
       _    -> errorTiposMsg p "El tipo de la variable y del valor no son iguales" tvar tval  
transExp(IfExp co th Nothing p) =
  do checkBreaks co
     checkBreaks th
     (_ , co') <- transExp co
     C.unless (equivTipo co' TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" co' TBool 
     (() , th') <- transExp th
     C.unless (equivTipo th' TUnit) $ errorTiposMsg p "La branch esta devolviendo un resultado" th' TUnit
     return (() , TUnit)
transExp(IfExp co th (Just el) p) = 
  do checkBreaks co
     checkBreaks th
     checkBreaks el
     (_ , condType) <- transExp co
     C.unless (equivTipo condType TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" condType TBool
     (_, ttType) <- transExp th
     (_, ffType) <- transExp el
     C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "Las branches devuelven resultados de distinto tipo" ttType ffType
     return ((), ttType)
transExp(WhileExp co body p) = 
  do checkBreaks co
     (_ , coTy) <- transExp co
     C.unless (equivTipo coTy TBool) $ errorTiposMsg p "La condicion del While no es booleana" coTy TBool
     (_ , boTy) <- transExp $ scanBreaks body
     C.unless (equivTipo boTy TUnit) $ errorTiposMsg p "El cuerpo del While devuelve un resultado" boTy TBool
     return ((), TUnit)
transExp(ForExp nv mb lo hi bo p) =
  do checkBreaks lo
     checkBreaks hi
     (_, tlo) <- transExp  lo
     C.unless (equivTipo tlo (TInt RW)) $ errorTiposMsg p "La cota inferior del for no es un entero modificable" tlo (TInt RW)
     (_, thi) <- transExp  hi
     C.unless (equivTipo thi (TInt RW)) $ errorTiposMsg p "La cota superior del for no es un entero modificable" thi (TInt RW)
     (_, tbo) <- insertValV nv (TInt RO) $ transExp (scanBreaks bo)
     C.unless (equivTipo tbo TUnit) $ errorTiposMsg p "El cuerpo del for está devolviendo un valor" tbo TUnit
     return ((), TUnit)
transExp(LetExp dcs body p) = 
  do checkBreaks body
     transDecs dcs $ transExp body
transExp(BreakExp p) = return ((), TUnit)
transExp(ArrayExp sn cant init p) =
  do checkBreaks cant
     checkBreaks init
     tsn <- getTipoT sn 
     case tsn of
       TArray ta _ -> do (_, tca) <- transExp cant
                         C.unless (equivTipo tca (TInt RO)) $ errorTiposMsg p "El indice no es un entero" tca (TInt RO)
                         (_, tin) <- transExp init
                         C.unless (equivTipo ta tin) $ errorTiposMsg p "El valor inicial no es del tipo del arreglo" ta tin
                         return ((), tsn)
       _           -> errorTiposMsg p "La variable no es de tipo arreglo" tsn (TArray TUnit 0)

-- checkBreaks se llamará en todos los casos de transExp,
-- excepto en WhileExp y ForExp
checkBreaks :: Manticore w => Exp -> w ((), Tipo)
checkBreaks (BreakExp p) = addpos (derror $ pack "Break fuera de loop") p 
checkBreaks _            = return ((), TUnit) 

-- scanBreaks reemplazará todas las ocurrencias de BreakExp por
-- UnitExp. scanBreaks se llamará en transExp para los casos de
-- WhileExp, y ForExp, de manera que el reemplazo se haga dentro
-- de los bucles.
scanBreaks :: Exp -> Exp
scanBreaks (CallExp s le p)          = CallExp s (P.map scanBreaks le) p
scanBreaks (OpExp el o er p)         = OpExp (scanBreaks el) o (scanBreaks er) p
scanBreaks (RecordExp es s p)        = RecordExp (P.map (\(sym, exp) -> (sym, scanBreaks exp)) es) s p
scanBreaks (SeqExp le p)             = SeqExp (P.map scanBreaks le) p
scanBreaks (AssignExp v e p)         = AssignExp v (scanBreaks e) p
scanBreaks (IfExp co th el p)  
  | isNothing el = IfExp (scanBreaks co) (scanBreaks th) Nothing p
  | otherwise    = IfExp (scanBreaks co) (scanBreaks th) (Just $ scanBreaks (fromJust el)) p
scanBreaks (WhileExp co bd p)        = WhileExp (scanBreaks co) (scanBreaks bd) p
scanBreaks (ForExp s esc lo hi bd p) = ForExp s esc (scanBreaks lo) (scanBreaks hi) (scanBreaks bd) p
scanBreaks (LetExp dcs e p)          = LetExp dcs (scanBreaks e) p
scanBreaks (ArrayExp s ct i p)       = ArrayExp s (scanBreaks ct) (scanBreaks i) p 
scanBreaks (BreakExp p)              = UnitExp p
scanBreaks ex                        = ex

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Estado inicial y ejecucion ---------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

initConf :: Estado
initConf = Est
           {tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
            , vEnv = M.fromList
                     [(pack "print", Func (1,pack "print",[TString], TUnit, Runtime)),
                      (pack "flush", Func (1,pack "flush",[],TUnit, Runtime)),
                      (pack "getchar",Func (1,pack "getchar",[],TString,Runtime)),
                      (pack "ord",Func (1,pack "ord",[TString],TInt RW,Runtime)),
                      (pack "chr",Func (1,pack "chr",[TInt RW],TString,Runtime)),
                      (pack "size",Func (1,pack "size",[TString],TInt RW,Runtime)),
                      (pack "substring",Func (1,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime)),
                      (pack "concat",Func (1,pack "concat",[TString,TString],TString,Runtime)),
                      (pack "not",Func (1,pack "not",[TBool],TBool,Runtime)),
                      (pack "exit",Func (1,pack "exit",[TInt RW],TUnit,Runtime))]}

runMonada :: Monada ((), Tipo) -> StGen (Either Symbol ((), Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol ((), Tipo))
runSeman = runMonada . transExp
