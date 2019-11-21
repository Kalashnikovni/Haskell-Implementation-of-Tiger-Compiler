module TigerTrans where

import qualified Control.Monad.State as ST
import Prelude hiding (EQ,
                       GT,
                       LT,
                       error,
                       exp,
                       seq)
import qualified Prelude as P (error, length)

import TigerAbs (Escapa(..), Oper(..))
import qualified TigerAbs as Abs
import TigerErrores
import TigerFrame as F
--import TigerSres (Externa(..))
import TigerSymbol as T
import TigerTemp
import TigerTree

import Control.Monad
import qualified Data.Foldable as Fold
import Data.List as List
import Data.Ord hiding (EQ,
                        GT,
                        LT)
import Debug.Trace

data Externa = Runtime | Propia deriving Show

-- | Reexportamos el tipo de Fragmentos provenientes de TigerTrans.
type TransFrag = Frag

-- | Tipo de datos representando si es un procedimiento o una función
data IsProc = IsProc | IsFun

-- | Empaquetadores de expresiones
-- Esto pasa ya que la información de contexto, es decir, donde están cada
-- una de las expresiones, statements y/o condicionales, lo sabemos
-- en el otro modulo, en [TigerSeman].
data BExp where
  -- | Representa una expresión. Es decir que se espera que devuelva
  -- algún resultado.
  Ex :: Exp -> BExp
  -- | Representan las computaciones que no dan resultados, es decir
  -- un /statement/
  Nx :: Stm -> BExp
  -- | Representan a expresiones que representan condicionales.
  Cx  :: ( -- | Dadas las etiquetas a donde saltar en caso de verdadero
           -- o falso.
          (Label, Label)
         -- | Y devolvemos un Statement formado correctamente.
          -> Stm)
      -> BExp

instance Show BExp where
    show (Ex e)  = "Ex " ++ show e
    show (Nx e)  = "Nx " ++ show e
    show (Cx _ ) = "Cx "

-- | Función helper /seq/ que nos permite escribir
-- fácilmente una secuencia de [Stm] usando listas.
seq :: [Stm] -> Stm
seq []       = ExpS $ Const 0
seq [s]      = s
seq (x : xs) = Seq x (seq xs)

-- | Eventualmente vamos a querer obtener nuevamente las expresiones
-- empaquetadas por este nuevo tipo [BExp]. Para eso damos las siguientes
-- funciones des-empaquetadoras. Definidas en [7.3] del libro.

-- | Des-empaquetador de expresiones
-- Es mónadico ya que deberá crear labels, y temps
-- para des-empaquetar una condición.
unEx :: (Monad w, TLGenerator w) => BExp -> w Exp
unEx (Ex e)  = return e
unEx (Nx s)  = return $ Eseq s (Const 0)
unEx (Cx cf) = 
  do r <- newTemp
     t <- newLabel
     f <- newLabel
     return $ Eseq (seq
                    [Move (Temp r) (Const 1),
                     cf (t, f),
                     Label f,
                     Move (Temp r) (Const 0),
                     Label t])
                   (Temp r)

-- | Des-empaquetador de statements
unNx :: (Monad w, TLGenerator w) => BExp -> w Stm
unNx (Ex e)  = return $ ExpS e
unNx (Nx s)  = return s
unNx (Cx cf) = 
  do t <- newLabel
     return $ seq [cf (t, t), Label t]

-- | Des-empaquetador de condiciones
unCx :: (Monad w, TLGenerator w, Demon w) => BExp -> w ((Label, Label) -> Stm)
unCx (Nx nx)        = internal $ pack $ "unCx(Nx ...): " ++ show nx
unCx (Cx cf)        = return cf
unCx (Ex (Const 0)) = return (\(_, f) -> Jump (Name f) f)
unCx (Ex (Const _)) = return (\(t, _) -> Jump (Name t) t)
unCx (Ex e)         = return (uncurry (CJump NE e (Const 0)))

-- | Los niveles son un stack de (Frame, Int)
-- Recordar que Frame es una representación del Marco Virtual.
data LevelI = MkLI {getFrame' :: Frame, getNlvl' :: Int}
  deriving Show

type Level = [LevelI]

-- | Helpers de niveles.
getFrame :: Level -> Frame
getFrame = getFrame' . head

getNlvl :: Level -> Int
getNlvl = getNlvl' . head

setFrame :: Frame -> Level -> Level
setFrame f (MkLI _ l : xs) = MkLI f l : xs
setFrame _ _               = P.error "setFrame"

newLevel :: Level -> Symbol -> [Escapa] -> Level
newLevel []                  s bs = [MkLI (newFrame s bs) 0]
newLevel ls@(MkLI _ lvl : _) s bs = MkLI (newFrame s bs) (lvl + 1) : ls

getParent :: Level -> Level
getParent []       = P.error "Nos fuimos del outermost level"
getParent (_ : xs) = xs

outermost :: Level
outermost = [MkLI (newFrame (pack "_undermain") []) (-1)]

-- | Clase encargada del manejo de memoria y niveles.
-- Esta etapa va a consumir el AST y construir un nuevo lenguaje llamado Código
-- Intermedio. En este proceso vamos tomando nota cuantas variables define una
-- función o let, para eventualmente crear los marcos necesarios para la
-- ejecución de código assembler.
class (Monad w, TLGenerator w, Demon w) => MemM w where
    -- | Level management
    -- Es un entero que nos indica en qué nivel estamos actualmente.
    getActualLevel :: w Int
    getActualLevel = getNlvl <$> topLevel
    upLvl :: w ()
    downLvl :: w ()
    -- | Salida management.
    -- Esta etiqueta la necesitamos porque es la que nos va permitir saltar a la
    -- salida de un while (Ver código intermedio de While). Usada en el break.
    pushSalida :: Maybe Label -> w ()
    topSalida :: w (Maybe Label)
    popSalida :: w ()
    -- | Level management Cont. El nivel en esta etapa es lo que llamamos el
    -- marco de activación virtual o dinámico (no me acuerdo). Pero es lo que
    -- eventualmente va a ser el marco de activación
    pushLevel :: Level -> w ()
    popLevel  :: w ()
    topLevel  :: w Level
    -- | Manejo de /pedido/ de memoria para variables locales.
    -- Esto básicamente debería aumentar en uno la cantidad de variables locales
    -- usadas. Es lo que se usará eventualmente para toquetear el stack o lo que
    -- sea que use la arquitectura deseada.
    allocLocal :: Escapa -> w Access
    allocLocal b = do
      -- | Dame el nivel actual
        t <- topLevel
        popLevel
      -- dame una versión modificada según lo dicte
      -- el módulo de manejo de Frame (que simula la arquitectura)
        (f,acc) <- F.allocLocal (getFrame t) b
      -- este nuevo frame es lo que vamos a usar.
        let nt = setFrame f t
        pushLevel nt
      -- y devolvemos el acceso creado. Si está en un temporal (registro) o en
      -- memoria (y en qué /offset/ del /fp/).
        return  acc
    -- | Manejo de /pedido/ de memoria para argumentos.
    -- ver lo que hicimos en /allocLocal/
    allocArg :: Escapa -> w Access
    allocArg b = do
        t <- topLevel
        popLevel
        (f,a) <- F.allocArg (getFrame t) b
        pushLevel (setFrame f t)
        return a
    -- | Frag management
    -- Básicamente los fragmentos van a ser un efecto lateral de la computación.
    -- Recuerden que los fragmentos son pedazos de código intermedio que se van
    -- a ejecutar. Y estos son un efecto lateral porque todavía no sabemos bien
    -- cómo van a ser ejecutados (eso se decide más adelante)
    pushFrag  :: Frag -> w ()
    getFrags  :: w [Frag]

-- | Generación de código intermedio.
-- Cada construcción del (AST)[src/TigerAbs.hs] la consumiremos
-- y construiremos un fragmento de código intermedio que eventualmente
--  se traducirá en código de máquina y ejecutará.
-- Algunas funciones se especializan más para conseguir un mejor código intermedio.
class IrGen w where
    -- | Esta función mágica prepara la máquina para comenzar a traducir una función o procedimiento.
    -- básicamente es la que va a agregar el Fragmento que es generado por la
    -- función y ponerlo como el efecto secundario mencionado más arriba
    procEntryExit :: Level -> BExp -> w ()
    simpleVar :: Access -> Int -> w BExp
    fieldVar :: BExp -> Int -> w BExp
    subscriptVar :: BExp -> BExp -> w BExp
    unitExp :: w BExp
    nilExp :: w BExp
    intExp :: Int -> w BExp
    stringExp :: Symbol -> w BExp
    varDec :: Access -> w BExp
    recordExp :: [(BExp,Int)]  -> w BExp
    callExp :: Label -> Externa -> IsProc -> Level -> [BExp] -> w BExp
    letExp :: [BExp] -> BExp -> w BExp
    breakExp :: w BExp
    seqExp :: [BExp] -> w BExp
    preWhileforExp :: w ()
    posWhileforExp :: w ()
    whileExp :: BExp -> BExp -> w BExp
    forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    ifThenExp :: BExp -> BExp -> w BExp
    ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    assignExp :: BExp -> BExp -> w BExp
    -- preFunctionDec :: Level -> w ()
    -- posFunctionDec :: w ()
    -- Esto fuerza a que haya menos opciones... ver bien con los que lleguen a
    -- este lugar..
    envFunctionDec :: Level -> w BExp -> w BExp
    functionDec :: BExp -> Level -> IsProc -> w BExp
    binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntRelExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    arrayExp :: BExp -> BExp -> w BExp

instance (MemM w) => IrGen w where
  procEntryExit lvl bd =  
    do bd' <- unNx bd
       let res = Proc bd' (getFrame lvl)
       pushFrag res
  -- lvl es nivel donde se usa la variable menos nivel donde se declaro
  simpleVar acc lvl  
    | lvl >= 0  =
      case exp acc lvl of
        Just e  -> 
          do tmpDec <- newTemp
             tmpOff <- newTemp
             case getOffset acc of
               Just k -> 
                 return $ Ex $ Eseq (seq [(Move (Temp tmpDec) e), 
                                          (Move (Temp tmpOff) (Temp tmpDec))]) 
                                    (Temp tmpOff)
               _      -> internal $ pack $ "Revisar el compilador.simpleVar, o revisar codigo del programa." ++ 
                                           "La variable no escapa."
        _ -> internal $ pack "Revisar compilador.simpleVar, o revisar codigo del programa."  
    | otherwise = internal $ pack $ "Revisar compilador.simpleVar, o revisar codigo del programa. La variable" ++
                                    "se usa en un nivel inferior a su declaracion."   
  fieldVar be i =
    do ebe  <- unEx be
       tbe  <- newTemp
       tres <- newTemp 
       return $ Ex $ Eseq (seq [Move (Temp tbe) ebe,
                                Move (Temp tres) $ Mem (Binop Plus (Temp tbe) (Binop Mul (Const i) (Const wSz)))])
                          (Temp tres)
  subscriptVar var ind = 
    do evar <- unEx var
       eind <- unEx ind
       tvar <- newTemp
       tind <- newTemp
       return $ Ex $ Eseq (seq [Move (Temp tvar) evar,
                                Move (Temp tind) eind,
                                ExpS $ externalCall "_checkIndex" [Temp tvar, Temp tind]])
                     (Mem $ Binop Plus (Temp tvar) (Binop Mul (Temp tind) (Const wSz)))
  stringExp t = 
    do -- | Esto debería ser dependiente de la arquitectura...
       -- No estoy seguro que tenga que estar esto acá.
       l <- newLabel
       let ln = T.append (pack ".long ")  (pack $ show $ T.length t)
       let str = T.append (T.append (pack ".string \"") t) (pack "\"")
       pushFrag $ AString l [ln,str]
       return $ Ex $ Name l
    -- | Función utilizada para la declaración de una función.
  envFunctionDec lvl funDec = 
    do -- preFunctionDec
       -- mandamos un nada al stack, por si un /break/ aparece en algún lado que
       -- no tenga un while y detectamos el error. Ver [breakExp]
       pushSalida Nothing
       upLvl
       pushLevel lvl
       fun <- funDec
       -- posFunctionDec
       -- | Cuando salimos de la función sacamos el 'Nothing' que agregamos en 'preFunctionDec'.
       popSalida
       downLvl
       -- devolvemos el código en el entorno donde fue computada.
       return fun
  -- functionDec :: BExp -> Level -> Bool -> w BExp
  functionDec bd lvl proc = 
    do body <- (case proc of
                  IsProc -> unNx bd
                  IsFun  -> Move (Temp rv) <$> unEx bd)
       procEntryExit lvl (Nx $ procEntryExit1 (getFrame lvl) body)
       return $ Ex $ Const 0
  varDec acc = simpleVar acc 0
  unitExp = return $ Ex (Const 0)
  nilExp = return $ Ex (Const 0)
  intExp i = return $ Ex (Const i)
  -- recordExp :: [(BExp,Int)]  -> w BExp
  -- ExpS $ externalCall "_checkIndex" [Temp tvar, Temp tind]])
  recordExp flds =
    do tmp   <- newTemp
       flds' <- mapM unEx $ map fst $ List.sortOn snd flds
       return $ Ex $ Eseq (Seq (ExpS (externalCall "_allocRecord" ((Const $ P.length flds) : flds')))
                               (Move (Temp tmp) (Temp rv)))  
                          (Temp tmp)
  callExp nm external isproc lvl args = 
    do lvlact <- getActualLevel
       args'  <- mapM (\x -> do x' <- unEx x
                                t  <- newTemp
                                return $ Eseq (Move (Temp t) x') (Temp t)) args
       case isproc of
         IsProc -> return $ Nx $ ExpS $ Call (Name nm) (auxexp (lvlact - defLvl) : args')
         IsFun  -> return $ Ex $ Call (Name nm) (auxexp (lvlact - defLvl) : args')
    where defLvl = getNlvl lvl 
  -- letExp :: [BExp] -> BExp -> w BExp
  letExp [] e = 
    do -- Des-empaquetar y empaquetar como un |Ex| puede generar
       -- la creación de nuevo temporales, etc. Es decir, hay efectos que necesitamos contemplar.
       -- Ver la def de |unEx|
       e' <- unEx e
       return $ Ex e'
  letExp bs body = 
    do bes <- mapM unNx bs
       be <- unEx body
       return $ Ex $ Eseq (seq bes) be
  -- breakExp :: w BExp
  breakExp =
    do lastM <- topSalida
       case lastM of
         Just l  -> return $ Nx $ Jump (Name l) l
         Nothing -> internal $ pack "Break fuera de loop"
  -- seqExp :: [BExp] -> w BExp
  seqExp [] = return $ Nx $ ExpS $ Const 0
  seqExp bes = 
    case last bes of
      Nx _  -> Nx . seq <$> mapM unNx bes
      Ex e' -> do let bfront = init bes
                  ess <- mapM unNx bfront
                  return $ Ex $ Eseq (seq ess) e'
      Cx c -> do c' <- unEx $ Cx c 
                 let bfront = init bes
                 ess <- mapM unNx bfront
                 return $ Ex $ Eseq (seq ess) c'  
  -- preWhileforExp :: w ()
  preWhileforExp = newLabel >>= pushSalida . Just
  -- posWhileforExp :: w ()
  posWhileforExp = popSalida
  whileExp cond body = 
    do ccond <- unCx cond
       cbody <- unNx body
       test  <- newLabel
       lbody <- newLabel
       lastM <- topSalida
       case lastM of
         Just done ->
           return $ Nx $ seq [Label test,
                              ccond (lbody, done),
                              Label lbody,
                              cbody,
                              Jump (Name test) test,
                              Label done]
         _ -> internal $ pack "no label in salida"
  forExp lo hi var body =
    do elo    <- unEx lo
       ehi    <- unEx hi
       thi    <- newTemp
       evar   <- unEx var
       cbody  <- unNx body
       tmp    <- newTemp
       test   <- newLabel
       lbody  <- newLabel
       lsigue <- newLabel
       lastM  <- topSalida
       case lastM of
         Just done ->
           return $ Nx $ seq [Move (Temp thi) ehi,
                              Move evar elo,
                              CJump LE evar (Temp thi) lbody done,
                              Label lbody,
                              cbody,
                              CJump EQ evar (Temp thi) done lsigue,
                              Label lsigue,
                              Move evar $ Binop Plus evar (Const 1),
                              Jump (Name lbody) lbody,
                              Label done]  
  ifThenExp cond bod =
    do ccond <- unCx cond
       nbod  <- unNx bod
       t     <- newLabel
       f     <- newLabel
       return $ Nx $ seq [ccond (t, f), Label t, nbod, Label f]
{-
  ifThenElseExp cond (Cx bod) (Cx els) = 
    do ccond <- unCx cond
       t     <- newLabel
       f     <- newLabel
       fin   <- newLabel
       return $ Nx $ seq [ccond (t, f), 
                          Label t, bod (fin, fin),   
                          Label f, els (fin, fin),
                          Label fin] 
  ifThenElseExp cond (Cx bod) els =
    do ccond <- unCx cond
       nels  <- unNx els
       t     <- newLabel
       f     <- newLabel
       fin   <- newLabel
       return $ Nx $ seq [ccond (t, f),
                          Label t, bod (fin, fin),
                          Label f, nels,
                          Label fin]
  ifThenElseExp cond bod (Cx els) =
    do ccond <- unCx cond
       nbod  <- unNx bod
       t     <- newLabel
       f     <- newLabel
       fin   <- newLabel
       return $ Nx $ seq [ccond (t, f),
                          Label t, nbod, Jump (Name fin) fin,
                          Label f, els (fin, fin),
                          Label fin]-}
  ifThenElseExp cond bod els =
    do ccond <- unCx cond
       ebod  <- unEx bod
       eels  <- unEx els
       t     <- newLabel
       f     <- newLabel
       fin   <- newLabel
       tmp   <- newTemp
       return $ Ex $ Eseq (seq [ccond (t, f),
                                Label t, Move (Temp tmp) ebod, Jump (Name fin) fin,
                                Label f, Move (Temp tmp) eels,
                                Label fin])
                          (Temp tmp)
  --ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
  ifThenElseExpUnit cond bod els = 
    do ccond <- unCx cond
       nbod  <- unNx bod
       nels  <- unNx els
       t     <- newLabel
       f     <- newLabel
       fin   <- newLabel
       return $ Nx $ seq [ccond (t, f),
                          Label t, nbod, Jump (Name fin) fin,
                          Label f, nels,
                          Label fin]      
  assignExp cvar cinit = 
    do cvara <- unEx cvar
       cin   <- unEx cinit
       case cvara of
         Mem v' -> do t <- newTemp
                      return $ Nx $ seq [Move (Temp t) cin, Move cvara (Temp t)]
         _ -> return $ Nx $ Move cvara cin
  binOpIntExp le op re =
    do leex <- unEx le
       reex <- unEx re
       op'  <- transformOp op
       return $ Ex $ Binop op' leex reex
    where transformOp PlusOp   = return Plus
          transformOp MinusOp  = return Minus
          transformOp TimesOp  = return Mul
          transformOp DivideOp = return Div
          transformOp _        = internal $ pack $ "Revisar el compilador.binOpIntExp o el codigo. El operador " ++
                                                   "no es aritmetico"
  binOpStrExp strl op strr =
    do esl   <- unEx strl
       esr   <- unEx strr
       tstrl <- newTemp
       tstrr <- newTemp
       let res = seq [Move (Temp tstrl) esl, 
                      Move (Temp tstrr) esr, 
                      ExpS $ externalCall "_stringCompare" [Temp tstrl, Temp tstrr],
                      Move (Temp tstrl) (Temp rv)]
           mkCx op = Cx (\(t, f) -> seq [res, CJump op (Temp tstrl) (Const 0) t f])
       case op of
         EqOp  -> return $ mkCx EQ
         NeqOp -> return $ mkCx NE
         LtOp  -> return $ mkCx LT 
         GtOp  -> return $ mkCx GT
         LeOp  -> return $ mkCx LE
         GeOp  -> return $ mkCx GE
         _     -> internal $ pack "Revisar el compilador TigerTrans.binOpStrExp"
  binOpIntRelExp le op re =
    do leex <- unEx le
       reex <- unEx re
       op'  <- transformOp op
       return $ Cx (\(t, f) -> CJump op' leex reex t f)
    where transformOp EqOp  = return EQ
          transformOp NeqOp = return NE
          transformOp LtOp  = return LT
          transformOp GtOp  = return GT
          transformOp LeOp  = return LE
          transformOp GeOp  = return GE
          transformOp _     = internal $ pack $ "Revisar el compilador.binOpIntRelExp o el codigo. El operador " ++
                                                "no es relacional"
  arrayExp size init = 
    do sz <- unEx size
       ini <- unEx init
       t <- newTemp
       return $ Ex $ Eseq (seq [ExpS $ externalCall "_allocArray" [sz, ini],
                                Move (Temp t) (Temp rv)]) 
                          (Temp t)
