
module TigerFrame where

import TigerAbs (Escapa(..))
import TigerAssem(Instr(..))
import TigerSymbol
import TigerTemp
import TigerTree

import Debug.Trace

import Data.Map

import Prelude as P hiding (exp)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Registros y convenciones de la arquitectura ----------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- | Registros especiales
fp, sp, lo, hi, zero, ra, rv0, rv1, gp :: Temp 
gp = pack "gp"
fp = pack "fp"
sp = pack "sp"
hi = pack "high"
lo = pack "low"
zero = pack "zero"
ra = pack "ra"
rv0 = pack "v0"
rv1 = pack "v1"

a0, a1, a2, a3 :: Temp
a0 = pack "a0"
a1 = pack "a1"
a2 = pack "a2"
a3 = pack "a3"

t0, t1, t2, t3, t4, t5, t6, t7, t8, t9 :: Temp 
t0 = pack "t0"
t1 = pack "t1"
t2 = pack "t2"
t3 = pack "t3"
t4 = pack "t4"
t5 = pack "t5"
t6 = pack "t6"
t7 = pack "t7"
t8 = pack "t8"
t9 = pack "t9"

s0, s1, s2, s3, s4, s5, s6, s7 :: Temp
s0 = pack "s0"
s1 = pack "s1"
s2 = pack "s2"
s3 = pack "s3"
s4 = pack "s4"
s5 = pack "s5"
s6 = pack "s6"
s7 = pack "s7"

-- | Listas de registros que define la llamada y registros especiales
calldefs, specialregs, argregs, callersaved :: [Temp]
argregs = [a0, a1, a2, a3]
calleesaved = [s0, s1, s2, s3, s4, s5, s6, s7]
callersaved = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9]
calldefs = [rv0, ra] ++ callersaved
specialregs = [fp, sp, hi, lo, zero, ra, rv0, rv1, gp]
allregs = argregs ++ calleesaved ++ callersaved ++ specialregs

argsRegsCount :: Int
argsRegsCount = 4

-- | Word size in bytes
wSz :: Int
wSz = 4

-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 2

-- Estos offsets se utilizan para el calculo de acceso de variables que escapan
-- (principalmente)
-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Donde se encuentra el FP del nivel anterior (no necesariamente el llamante?)
fpPrevLev :: Int
fpPrevLev = 0

-- | Esto es un offset previo a al lugar donde se encuentra el lugar de las variables
-- o de los argumentos.
argsGap, localsGap :: Int
argsGap = wSz
localsGap = 4

-- | Dan inicio a los contadores de argumentos, variables y registros usados.
-- Ver |defaultFrame|
argsInicial, regInicial, localsInicial :: Int
argsInicial = 0
regInicial = 1
localsInicial = 0


-- | Tipo de dato que define el acceso a variables.
data Access =
  -- | En memoria, acompañada de una dirección
  InFrame Int
  -- | En un registro
  | InReg Temp
    deriving Show

getOffset :: Access -> Maybe Int
getOffset (InFrame k) = Just k
getOffset _           = Nothing

-- | Definición de fragmento usado en en la traducción.
-- Son los bloques que van al assembler de formal individual.
data Frag =
  -- | Es un procedimiento (recordar que ahora todo es un procedimiento)
  -- ya que el resultado viene como un efecto lateral en el |rv|
  Proc Stm Frame
  -- | Es una cadena de caracteres literal, en general esto va en el segmento de datos del assembler.
  | AString Label [Symbol]

-- | Función que nos permite separar los procedimientos y las cadenas de caracteres.
sepFrag :: [Frag] -> ([Frag], [(Stm, Frame)])
sepFrag xs = (reverse ass, reverse stmss)
 where
  (ass, stmss) = P.foldl
    (\(lbls, stms) x -> case x of
      Proc st fr -> (lbls, (st, fr) : stms)
      AString{}  -> (x : lbls, stms)
    )
    ([], [])
    xs

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = show l ++ ":\n" ++ (P.foldr (\t ts -> ("\n\t" ++ unpack t) ++ ts) "" ts)

-- | |Frame| es lo que representa el marco de activación dinámico, es la
-- información que vamos a utilizar eventualmente para construir el marco de
-- activación real al momento de efectuar las llamadas a funciones. Que consiste en:
data Frame = Frame {
        -- | Nombre que lleva en el assembler.
        name        :: Symbol,
        -- | Argumentos, si escapan o no.
        formals     :: [Escapa],
        -- | Variables Locales , si escapan o no.
        locals      :: [Escapa],
        -- | Contadores de cantidad de argumentos, variables y registros.
        actualArg   :: Int,
        actualLocal :: Int,
        actualReg   :: Int
    }
    deriving Show
-- Nota: claramente pueden no llevar contadores y calcularlos en base a la longitud de
-- las listas |formals| y |locals|.

defaultFrame :: Frame
defaultFrame = Frame
  {name        = TigerSymbol.empty,
   formals     = [],
   locals      = [],
   actualArg   = argsInicial,
   actualLocal = localsInicial,
   actualReg   = regInicial}

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- TODOS A stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = reverse $ snd
  (P.foldl (\(n, rs) _ -> (n + argsGap, InFrame n : rs))
         (argsInicial, [])
         (formals fs)
  )

newFrame :: Symbol -> [Escapa] -> Frame
newFrame nm fs = defaultFrame {name = nm, formals = fs}

-- | Función auxiliar que hace una llamada externa.
externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

-- | A medida que vamos procesando los argumentos vamos pidiendo 'memoria' para ellos.
-- Dependiendo de la arquitectura algunos pueden ir por memoria o por stack. Salvo obviamente
-- que escapen, en ese caso tienen que ir a memoria.
allocArg :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocArg fr Escapa =
  let actual = actualArg fr
      acc    = InFrame $ actual * wSz + argsGap
  in  return (fr{actualArg = actual + 1}, acc)
allocArg fr NoEscapa = do
  s <- newTemp
  return (fr{actualReg = actualReg fr + 1}, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ actual * wSz + localsGap
  in  return (fr {actualLocal = actual + 1, locals = Escapa : (locals fr)}, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr{actualLocal = actualLocal fr + 1, locals = NoEscapa : (locals fr)}, InReg s)

-- Función auxiliar para el calculo de acceso a una variable, siguiendo el Static Link.
-- Revisar bien antes de usarla, pero ajustando correctamente la variable |fpPrevLev|
-- debería estar relativamente cerca de la solución
auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))

exp :: Access -> Int -> Maybe Exp
exp (InFrame k) e = Just $ Mem (Binop Plus (auxexp e) (Const k))
exp (InReg l) c 
  | c /= 0    = Nothing
  | otherwise = Just $ Temp l

procEntryExit1 :: Frame -> Stm -> Stm
procEntryExit1 fr body = body 

procEntryExit2 :: Frame -> [Instr] -> [Instr]
procEntryExit2 fr instrs =
  instrs ++ [Oper{assem = "", src = [zero, ra, sp, gp, fp, rv0, rv1, hi, lo, ra] ++ calleesaved, 
             dst = [], jump = Nothing}] 

data FrameFunc = FF {prolog :: String, body :: [Instr], epilogue :: String}
  deriving Show

procEntryExit3 :: Frame -> [Instr] -> FrameFunc
procEntryExit3 fr bd = 
  FF{prolog =  "PROCEDURE " ++ unpack (name fr) ++ "\n",
     body = bd ++ [Oper{assem = "jr `s0\n", dst = [], src = [ra], jump = Nothing}],
     epilogue = "END " ++ unpack (name fr) ++ "\n"}
