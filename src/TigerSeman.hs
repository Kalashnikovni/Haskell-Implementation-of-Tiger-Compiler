module TigerSeman where

import MonadsInstances
import State
import TigerAbs
import TigerCanon
import TigerErrores as E
import TigerSres
import TigerSymbol
import TigerTips
import TigerTopSort
import TigerUnique
import TigerTemp
import TigerTrans

-- Monads
import qualified Control.Conditional as C
import Control.Monad (foldM, zipWithM_)
import Control.Monad.State (evalStateT, runStateT)
import Control.Monad.Trans.Except

-- Data
import Data.List as List
import Data.Map as M
import Data.Maybe
import Data.Ord as Ord
import Data.Stack

import Debug.Trace

import Prelude as P

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Helpers ----------------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

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

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe (Tipo, Int)
buscarM s []              = Nothing
buscarM s ((s', t, p):xs) 
  | s == s'   = Just (t, p)
  | otherwise = buscarM s xs

cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _  = derrorAux "Tienen distintos campos - TigerSeman.cmpZip1\n"
cmpZip _ []  = derrorAux "Tienen distintos campos - TigerSeman.cmpZip2\n"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de variables ------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar (SimpleVar s)      = 
  do (t, acc, vLvl) <- getTipoValV s
     actLvl         <- getActualLevel
     bexp           <- simpleVar acc (actLvl - vLvl) 
     return (bexp, t)
transVar (FieldVar v s)     =
  do (bexp, t) <- transVar v
     case t of
       TRecord l _ -> maybe (derrorAux "Se intenta acceder a un campo que no pertenece al record") 
                            (\(tx, px) -> do bexp' <- fieldVar bexp px  
                                             return (bexp', tx))
                            (buscarM s l) 
       _           -> derrorAux "Se intenta acceder al campo de una variable que no es un record"
transVar (SubscriptVar v e) =
  do (bexpe, te) <- transExp e 
     case te of
       TInt _ -> do (bexpv, tv) <- transVar v
                    case tv of
                      TArray ta _ -> do bexp <- subscriptVar bexpv bexpe
                                        return (bexp, ta)
                      _           -> derrorAux "Se intenta indexar algo que no es un arreglo"
       _      -> derrorAux "El indice del arreglo no es un número"

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de tipos ----------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = getTipoT s
transTy (RecordTy flds) =
  do u   <- ugen
     res <- foldM (\((TRecord lf u'), p) (s, t) -> 
                    do t' <- transTy t
                       return (TRecord ((s, t', p):lf) u', p + 1)) ((TRecord [] u), 0) flds'
     return $ fst res
  where flds' = List.sortBy (Ord.comparing fst) flds
transTy (ArrayTy s)     =
  do ts <- getTipoT s
     u <- ugen
     return $ TArray ts u

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Traduccion de declaraciones --------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

--transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transDecs :: (MemM w, Manticore w) => [Dec] -> w (BExp, Tipo) -> w ([BExp], BExp, Tipo)
transDecs [] w                               = 
  do (b, t) <- w
     return ([], b, t)
transDecs ((VarDec nm escap t init p) : xs) w = 
  do (bini, ti) <- transExp init
     case t of
       Just sv -> do tv <- getTipoT sv
                     C.unless (equivTipo tv ti) $ errorTiposMsg p "El tipo del valor inicial es incorrecto" tv ti
                     acc   <- allocLocal escap
                     bvar  <- varDec acc
                     res   <- assignExp bvar bini
                     lvl   <- getActualLevel
                     (lbexp, bb, bt) <- insertValV nm (tv, acc, lvl) $ transDecs xs w
                     return $ (res : lbexp, bb, bt)
       Nothing -> do C.when (ti == TNil) $ errorTiposMsg p "Debe usar la forma extendida" ti TNil
                     acc   <- allocLocal escap
                     bvar  <- varDec acc
                     res   <- assignExp bvar bini
                     lvl   <- getActualLevel
                     (lbexp, bb, bt) <- insertValV nm (ti, acc, lvl) $ transDecs xs w
                     return $ (res : lbexp, bb, bt)
transDecs ((FunctionDec fs) : xs)          w =
  do res1 <- mapM (\f@(nm, params, tf, bd, p) -> 
               do actLvl <- topLevel
                  l <- newLabel
                  let flabel = pack $ unpack l ++ "-" ++ unpack nm 
                  let lvlFun = newLevel actLvl flabel (P.map (\(_, x, _) -> x) params) 
                  tfun <- maybe (return TUnit) (\t -> getTipoT t) tf
                  targs <- mapM (\(_, _, c) -> transTy c) params
                  return (lvlFun, flabel, targs, tfun, TigerSres.Propia)) fs
     mapM_ (\((nm, params, tf, bd, p), (lvlFun, _, _, _, _))->
            envFunctionDec lvlFun $ 
              insertFFold fs res1 (do lvlArgs <- getActualLevel
                                      allocArg Escapa -- TODO: ver si escapa
                                      insertFFFold params lvlArgs $ 
                                        do lvlArgs' <- topLevel
                                           (bf, t) <- transExp bd
                                           case tf of
                                             Just td -> 
                                               do tdd <- getTipoT td
                                                  C.unless (equivTipo t tdd) $
                                                    errorTiposMsg p auxstring t tdd
                                                  functionDec bf lvlArgs' IsFun
                                             Nothing -> 
                                               do C.unless (equivTipo t TUnit) $
                                                    errorTiposMsg p "La funcion devuelve un valor" t TUnit
                                                  functionDec bf lvlArgs' IsProc)) (zip fs res1)
     insertFFold fs res1 $ transDecs xs w
  where auxstring = "El valor retornado no es del tipo declarado"
transDecs ((TypeDec xs) : xss)             w =
  let 
    checkNames [] w'     = w' 
    checkNames (z:zs) w' = 
      case elem (fst z) (P.map fst zs) of
        False -> checkNames zs w'
        True  -> addpos (derrorAux "Hay dos tipos con el mismo nombre en un batch") (snd z) 
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

insertFFold :: (MemM w, Manticore w) => 
               [(Symbol, [(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)] -> 
               [FunEntry] ->
               w a -> w a
insertFFold [] _ w                                      = w
insertFFold ((nm, params, res, bd, p) : fs) (a : aux) w =
  do case elem nm $ P.map fst5 fs of
       False -> insertFunV nm a $ insertFFold fs aux w
       True -> addpos (derrorAux "Hay dos funciones con el mismo nombre en un batch") p 
  where fst5 (a, _, _, _, _) = a

insertFFFold :: (MemM w, Manticore w) => [(Symbol, Escapa, Ty)] -> Int -> w a -> w a
insertFFFold [] l w                    = w
insertFFFold ((nm, e, t) : params) l w =
  do tt <- transTy t
     acc <- allocArg e
     insertValV nm (tt, acc, l) $ insertFFFold params l w

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
       _             -> internalAux "Error de Haskell, recordTy no tiene tipo TRecord"

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

transExp :: (MemM w, Manticore w) => Exp -> w (BExp, Tipo)
transExp (VarExp v p) = 
  addpos (transVar v) p
transExp UnitExp{} = 
  fmap (,TUnit) unitExp
transExp NilExp{} = 
  fmap (,TNil) nilExp
transExp (IntExp i _) = 
  fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = 
  fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) =
  do (lvl, lab, tfargs, tf, ext) <- getTipoFunV nm
     targs <- mapM transExp args
     C.when (P.length tfargs /= P.length targs) (addpos (derrorAux msg) p)
     zipWithM_ (\ta tb -> do C.unless (equivTipo ta (snd tb)) 
                                      (errorTiposMsg p "No coincide el tipo de los argumentos" ta (snd tb))) tfargs targs 
     res <- callExp lab (aux ext) (isproc tf) lvl (P.map fst targs) 
     return (res, tf)
  where isproc $ UnitExp _ = IsProc
        isproc _           = IsFun
        aux TigerSres.Runtime = TigerTrans.Runtime
        aux TigerSres.Propia  = TigerTrans.Propia
        msg = "La cantidad de argumentos pasados no coincide con la cantidad de argumentos en la declaracion"
transExp (OpExp el' oper er' p) = 
  do (resl, el) <- transExp el'
     (resr, er) <- transExp er'
     case oper of
       EqOp  -> if tiposComparables el er EqOp 
                then do (_, t) <- eqOps el er
                        res <- binOpIntRelExp resl EqOp resr
                        return (res, TInt RO)
                else errmsg "Error de tipos. Tipos no comparables 1:" el er
       NeqOp -> if tiposComparables el er EqOp 
                then do (_, t) <- eqOps el er
                        res <- binOpIntRelExp resl NeqOp resr
                        return (res, TInt RO)
                else errmsg "Error de tipos. Tipos no comparables 2:" el er
       PlusOp -> oOps el resl er resr PlusOp
       MinusOp -> oOps el resl er resr MinusOp
       TimesOp -> oOps el resl er resr TimesOp
       DivideOp -> oOps el resl er resr DivideOp
       LtOp -> ineqOps el resl er resr LtOp
       LeOp -> ineqOps el resl er resr LeOp
       GtOp -> ineqOps el resl er resr GtOp
       GeOp -> ineqOps el resl er resr GeOp
  where errmsg msg t1 t2 = addpos (derror $ pack $ msg ++ " " ++ show t1 ++ " " ++ show t2) p
        eqOps TNil TNil = errmsg "Error de tipos. Tipos no comparables 3:" TNil TNil -- Redundant
        eqOps TNil r    = if equivTipo TNil r
                          then do resu <- unitExp
                                  return (resu, TInt RO)
                          else errmsg "Error de tipos. Tipos no comparables 4:" TNil r 
        eqOps l TNil    = if equivTipo l TNil
                          then do resu <- unitExp
                                  return (resu, TInt RO)
                          else errmsg "Error de tipos. Tipos no comparables 5:" l TNil 
        eqOps l r = 
          case (equivTipo l r, equivTipo l (TInt RO), equivTipo l TString) of
            (True, True, _) -> do resu <- unitExp
                                  return (resu, TInt RO)
            (True, _, True) -> do resu <- unitExp
                                  return (resu, TString)
            (True, _, _)    -> if equivTipo l r
                               then do resu <- unitExp
                                       return (resu, TInt RO)
                               else errmsg "Error de tipos. Tipos no comparables 6:" l r
            _               -> errmsg "Error de tipos. Tipos no comparables 7: " l r
        oOps l rl r rr op = if equivTipo l r 
                               && equivTipo l (TInt RO) 
                            then do res <- binOpIntExp rl op rr
                                    return (res, TInt RO)
                            else errmsg "Error de tipos. Tipos no comparables 8:" l r
        ineqOps l rl r rr op = 
          case equivTipo l r of
            True ->
              case equivTipo l (TInt RO) of
                True  -> do res <- binOpIntRelExp rl op rr
                            return (res, TInt RO) 
                False ->
                  case equivTipo l TString of
                    True  -> do res <- binOpStrExp rl op rr
                                return (res, TInt RO) 
                    False -> errmsg "Error de tipos. Tipos no comparables 9:" l r
            False -> errmsg "No se pueden comparar los tipos 2: " l r
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \x -> case x of 
    trec@(TRecord fldsTy _) -> 
      do fldsTys <- mapM (\(nm, cod) -> do cod' <- transExp cod
                                           return (nm, fst cod', snd cod')) flds 
         let ordered  = List.sortBy (Ord.comparing fst3) fldsTys
             ordered' = List.sortBy (Ord.comparing fst3) fldsTy
         _ <- flip addpos p $ cmpZip ((\(s, c, t) -> (s,t)) <$> ordered) ordered' 
         res <- recordExp (zip (P.map snd3 ordered) [0..])
         return (res, trec) 
    t -> errorTiposMsg p "La variable no es de tipo record" t (TRecord [] 0)
  where fst3 (a, _, _) = a
        snd3 (_, b, _) = b
transExp(SeqExp es p) = 
  do bes <- mapM transExp es
     res <- seqExp (P.map fst bes)
     return (res, snd $ last bes)
transExp(AssignExp var val p) =
  do (bvar, tvar) <- transVar var
     (bval, tval) <- transExp val
     case equivTipo tvar tval of
       True -> do res <- assignExp bvar bval
                  return (res, TUnit) 
       _    -> errorTiposMsg p "El tipo de la variable y del valor no son iguales" tvar tval  
transExp(IfExp co th Nothing p) =
  do (bco, co') <- transExp co
     C.unless (equivTipo co' TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" co' TBool 
     (bth, th') <- transExp th
     C.unless (equivTipo th' TUnit) $ errorTiposMsg p "La branch esta devolviendo un resultado" th' TUnit
     res <- ifThenExp bco bth
     return (res , TUnit)
transExp(IfExp co th (Just el) p) = 
  do (bco, condType) <- transExp co
     C.unless (equivTipo condType TBool) $ errorTiposMsg p "El tipo de la condicion no es booleano" condType TBool
     (bth, ttType) <- transExp th
     (bel, ffType) <- transExp el
     C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "Las branches devuelven resultados de distinto tipo" ttType ffType
     case ttType of
       TUnit -> do res <- ifThenElseExpUnit bco bth bel
                   return (res, ttType)
       _     -> do res <- ifThenElseExp bco bth bel
                   return (res, ttType)
transExp(WhileExp co body p) = 
  do (bco, coTy) <- transExp co
     C.unless (equivTipo coTy TBool) $ errorTiposMsg p "La condicion del While no es booleana" coTy TBool
     preWhileforExp
     (bbody, boTy) <- transExp body
     C.unless (equivTipo boTy TUnit) $ errorTiposMsg p "El cuerpo del While devuelve un resultado" boTy TBool
     res <- whileExp bco bbody
     posWhileforExp
     return (res, TUnit)
transExp(ForExp nv mb lo hi bo p) =
  do (blo, tlo) <- transExp  lo
     C.unless (equivTipo tlo (TInt RW)) $ errorTiposMsg p "La cota inferior del for no es un entero modificable" tlo (TInt RW)
     (bhi, thi) <- transExp  hi
     C.unless (equivTipo thi (TInt RW)) $ errorTiposMsg p "La cota superior del for no es un entero modificable" thi (TInt RW)
     preWhileforExp
     vacc <- allocLocal Escapa
     bnv <- simpleVar vacc 0
     nvlvl <- getActualLevel
     (bbody, tbo) <- insertVRO nv (TInt RO, vacc, nvlvl) $ transExp bo
     C.unless (equivTipo tbo TUnit) $ errorTiposMsg p "El cuerpo del for está devolviendo un valor" tbo TUnit
     res <- forExp blo bhi bnv bbody
     posWhileforExp
     return (res, TUnit)
transExp(LetExp dcs body p) = 
  do (bs, bb, tb) <- transDecs dcs $ transExp body
     res <- letExp bs bb
     return (res, tb)
transExp(BreakExp p) = 
  do res <- breakExp
     return (res, TUnit)
transExp(ArrayExp sn cant init p) =
  do tsn <- getTipoT sn 
     case tsn of
       TArray ta _ -> do (bca, tca) <- transExp cant
                         C.unless (equivTipo tca (TInt RO)) $ errorTiposMsg p "El indice no es un entero" tca (TInt RO)
                         (bin, tin) <- transExp init
                         C.unless (equivTipo ta tin) $ errorTiposMsg p "El valor inicial no es del tipo del arreglo" ta tin
                         res <- arrayExp bca bin
                         return (res, tsn)
       _           -> errorTiposMsg p "La variable no es de tipo arreglo" tsn (TArray TUnit 0)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Estado inicial y ejecucion ---------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

initConf :: Estado
initConf = Est
           {tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
            , vEnv = M.fromList
                     [(pack "print", Func (outermost,pack "print",[TString], TUnit, TigerSres.Runtime)),
                      (pack "flush", Func (outermost,pack "flush",[],TUnit, TigerSres.Runtime)),
                      (pack "getchar",Func (outermost,pack "getchar",[],TString, TigerSres.Runtime)),
                      (pack "ord",Func (outermost,pack "ord",[TString],TInt RW, TigerSres.Runtime)),
                      (pack "chr",Func (outermost,pack "chr",[TInt RW],TString, TigerSres.Runtime)),
                      (pack "size",Func (outermost,pack "size",[TString],TInt RW, TigerSres.Runtime)),
                      (pack "substring",Func (outermost,pack "substring",[TString,TInt RW, TInt RW],TString, TigerSres.Runtime)),
                      (pack "concat",Func (outermost,pack "concat",[TString,TString],TString, TigerSres.Runtime)),
                      (pack "not",Func (outermost,pack "not",[TBool],TBool, TigerSres.Runtime)),
                      (pack "exit",Func (outermost,pack "exit",[TInt RW],TUnit, TigerSres.Runtime))]
            , lvl = [outermost]
            , lvlNum = 0
            , exitLabs = stackNew
            , frags = []}

runMonada1 :: Monada (BExp, Tipo) -> StGen (Either Symbol (BExp, Tipo))
runMonada1 =  flip evalStateT initConf . runExceptT

runSeman = runMonada1 . transExp  

transProg :: (MemM w, Manticore w) => Exp -> w [TransFrag]
transProg e = 
  do (be, _) <- transExp (LetExp [FunctionDec [(pack "_tigermain", 
                                                [], 
                                                Just $ pack "int", 
                                                e, 
                                                startPos)]] (IntExp 0 startPos) startPos)
     l <- topLevel
     f <- getFrags
     return $ reverse f
  where startPos = Simple{line = 0, col = 0}

runMonada2 :: Monada [TransFrag] -> StGen (Either Symbol [TransFrag])
runMonada2 = flip evalStateT initConf . runExceptT

runTransProg = runMonada2 . transProg
