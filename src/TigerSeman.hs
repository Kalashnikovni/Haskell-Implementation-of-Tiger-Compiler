module TigerSeman where

import Data.List as List
import Data.Ord  as Ord
import Prelude   as P

-- Monads
import qualified Control.Conditional as C
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except

import TigerAbs
import TigerEnv     as Env
import TigerErrores as Err
import TigerSres
import TigerSymbol
import TigerTips
import TigerUnique

-- Segunda parte imports:
import TigerTemp
-- import           TigerTrans

-- * Análisis Semántico, aka Inferidor de Tipos

-- \\\\\\\\\\\\\\\\\\\\\\\\\
-- Definimos algunos helpers -------------------------------------------------------------------------------------
-- /////////////////////////

depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts

-- | Función auxiliar que chequea cuales son los tipos comparables.
-- Por ejemplo, `if nil = nil then ...` es una expresión ilegal
-- ya que no se puede determinar el tipo de cada uno de los nils.
-- Referencia: [A.3.Expressions.Nil]
tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil TNil EqOp  = False
tiposComparables TUnit _ EqOp    = False
tiposComparables _ _ EqOp        = True
tiposComparables TNil TNil NeqOp = False
tiposComparables TUnit _ NeqOp   = False
tiposComparables _ _ NeqOp       = True
tiposComparables _ _ _           = True

-- | Función que chequea que los tipos de los campos sean los mismos
-- Ver 'transExp (RecordExp ...)'
-- Ver 'transExp (CallExp ...)'
cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _  = derror $ pack "Diferencia en la cantidad. 1"
cmpZip _ []  = derror $ pack "Diferencia en la cantidad. 2"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- \\\\\\\\\\\\\\
-- END OF HELPERS ------------------------------------------------------------------------------------------------
-- //////////////

-- \\\\\\\\\\\\\\\\\\\\\\\
-- Funciones de traduccion ---------------------------------------------------------------------------------------
-- ///////////////////////

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
-- ** transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s)      = undefined -- Nota [1]
transVar (FieldVar v s)     = undefined
transVar (SubscriptVar v e) = undefined

-- | __Completar__ 'TransTy'
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramatica, dar una representación
-- de tipo interna del compilador

-- | Nota para cuando se generte código intermedio
-- que 'TransTy ' no necesita ni 'MemM ' ni devuelve 'BExp'
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = undefined
transTy (RecordTy flds) = undefined
transTy (ArrayTy s)     = undefined


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."

-- | Tip: Capaz que se debería restringir el tipo de 'transDecs'.
-- Tip2: Van a tener que pensar bien que hacen. Ver transExp (LetExp...)
-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a
transDecs :: (Manticore w) => [Dec] -> w a -> w a
transDecs ((FunctionDec fs) : xs)          = id
transDecs ((VarDec nm escap t init p): xs) = id
transDecs ((TypeDec xs): xss)              = id

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p)    = Err.addpos (transVar v) p
transExp UnitExp{}       = return ((), TUnit) -- ** fmap (,TUnit) unitExp
transExp NilExp{}        = return ((), TNil) -- ** fmap (,TNil) nilExp
transExp (IntExp i _)    = return ((), TInt RW) -- ** fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = return (() , TString) -- ** fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) = undefined -- Completar
transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
        (_ , el) <- transExp el'
        (_ , er) <- transExp er'
        case oper of
          EqOp -> if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          NeqOp ->if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
          -- tendrán que hacer algo más interesante.
          PlusOp -> oOps el er
          MinusOp -> oOps el er
          TimesOp -> oOps el er
          DivideOp -> oOps el er
          LtOp -> oOps el er
          LeOp -> oOps el er
          GtOp -> oOps el er
          GeOp -> oOps el er
          where oOps l r = if equivTipo l r -- Chequeamos que son el mismo tipo
                              && equivTipo l (TInt RO) -- y que además es Entero. [Equiv Tipo es una rel de equiv]
                           then return ((), TInt RO)
                           else addpos (derror (pack "Error en el chequeo de una comparación.")) p
-- | Recordemos que 'RecordExp :: [(Symbol, Exp)] -> Symbol -> Pos -> Exp'
-- Donde el primer argumento son los campos del records, y el segundo es
-- el texto plano de un tipo (que ya debería estar definido). Una expresión
-- de este tipo está creando un nuevo record.
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \case -- Buscamos en la tabla que tipo es 'rt', y hacemos un análisis por casos.
    trec@(TRecord fldsTy _) -> -- ':: TRecord [(Symbol, Tipo, Int)] Unique'
      do
        -- Especial atención acá.
        -- Tenemos una lista de expresiones con efectos
        -- y estos efectos tiene producirse en orden! 'mapM' viene a mano.
        fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds -- Buscamos los tipos de cada una de los campos.
        -- como resultado tenemos 'fldsTys :: (Symbol, ( CIr , Tipo))'
        -- Lo que resta es chequear que los tipos  sean los mismos, entre los que el programador dio
        -- y los que tienen que ser según la definición del record.
        let ordered = List.sortBy (Ord.comparing fst) fldsTys
        -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
        _ <- flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> ordered) fldsTy -- Demon corta la ejecución.
        return ((), trec) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos.")
transExp(SeqExp es p) = fmap last (mapM transExp es)
  -- last <$> mapM transExp es
-- ^ Notar que esto queda así porque no nos interesan los
-- units intermedios. Eventualmente vamos a coleccionar los códigos intermedios y se verá algo similar a:
-- do
--       es' <- mapM transExp es
--       return ( () , snd $ last es')
transExp(AssignExp var val p) = error "Completar"
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (_ , co') <- transExp co
  -- chequeamos que sea un entero.
        unless (equivTipo co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (() , th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless (equivTipo th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        return (() , TUnit)
transExp(IfExp co th (Just el) p) = do
  (_ , condType) <- transExp co
  unless (equivTipo condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (_, ttType) <- transExp th
  (_, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  return ((), ttType)
transExp(WhileExp co body p) = do
  (_ , coTy) <- transExp co
  unless (equivTipo coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  (_ , boTy) <- transExp body
  unless (equivTipo boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TBool
  return ((), TUnit)
transExp(ForExp nv mb lo hi bo p) = error "Completar" -- Completar
transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) = return ((), TUnit)
transExp(ArrayExp sn cant init p) = error "Completar" -- Completar

-- \\\\\\\\\\\\\\\\\\\
-- END OF TRADUCCIONES -------------------------------------------------------------------------------------------
-- ///////////////////

runMonada :: Monada ((), Tipo)-> StGen (Either Symbol ((), Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol ((), Tipo))
runSeman = runMonada . transExp
