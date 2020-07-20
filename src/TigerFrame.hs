module TigerFrame where

import TigerAbs (Escapa(..))
import TigerAssem as A (Instr(..))
import TigerSymbol
import TigerTemp
import TigerTree as T

import Debug.Trace

import Data.Map

import Prelude as P hiding (exp)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Registros y convenciones de la arquitectura ----------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

-- | Registros especiales
--fp, sp, lo, hi, zero, ra, rv0, rv1, gp :: Temp 
sp, fp, rv :: Temp 
sp = pack "rsp"
fp = pack "rbp"
rv = pack "rax"

a0, a1, a2, a3, a5 :: Temp
a0 = pack "rdi"
a1 = pack "rsi"
a2 = pack "rdx"
a3 = pack "rcx"
a4 = pack "r8"
a5 = pack "r9"

r0, r1, r2, r3, r4, r5, r6, r7 :: Temp
r0 = pack "rax"
r1 = pack "rbx"
r2 = pack "r10"
r3 = pack "r11"
r4 = pack "r12"
r5 = pack "r13"
r6 = pack "r14"
r7 = pack "r15"

-- | Listas de registros que define la llamada y registros especiales
calldefs, specialregs, argregs, callersaved :: [Temp]
argregs = [a0, a1, a2, a3, a4, a5]
calleesaved = [r1, r4, r5, r6, r7]
callersaved = [rv, r2, r3]
calldefs = callersaved
specialregs = [fp, sp]
usablecolors = calleesaved ++ argregs ++ callersaved
allregs = argregs ++ calleesaved ++ callersaved ++ specialregs

argsRegsCount :: Int
argsRegsCount = P.length argregs

-- | Word size in bytes
wSz :: Int
wSz = 8

-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 3

-- Estos offsets se utilizan para el calculo de acceso de variables que escapan
-- (principalmente)
-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Donde se encuentra el FP del nivel anterior (no necesariamente el llamante?)
fpPrevLev :: Int
fpPrevLev = wSz

-- | Esto es un offset previo a al lugar donde se encuentra el lugar de las variables
-- o de los argumentos.
argsGap, localsGap :: Int
argsGap = wSz
localsGap = wSz

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
        formalsAcc  :: [Access],
        -- | Variables Locales , si escapan o no.
        locals      :: [Escapa],
        -- | Contadores de cantidad de argumentos, variables y registros.
        actualArg   :: Int,
        actualLocal :: Int,
        actualReg   :: Int
    }
    deriving Show

defaultFrame :: Frame
defaultFrame = Frame
  {name        = TigerSymbol.empty,
   formals     = [],
   formalsAcc  = [],
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
         (formals fs))

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
  in  return (fr{actualArg = actual + 1, formalsAcc = formalsAcc fr ++ [acc]}, acc)
allocArg fr NoEscapa = do
  s <- newTemp
  return (fr{actualReg = actualReg fr + 1, formalsAcc = formalsAcc fr ++ [InReg s]}, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ actual * wSz + localsGap
  in  return (fr {actualLocal = actual + 1, locals = Escapa : (locals fr)}, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr{locals = NoEscapa : (locals fr)}, InReg s)

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

procEntryExit1 :: (Monad w, TLGenerator w) => Frame -> Stm -> w Stm
procEntryExit1 fr body = 
  do (pre, post) <- allocCallee 
     return $ fseq $ adjFp ++ adjustArgs (formalsAcc fr) (tail argregs) 0 ++ pre ++ [body] ++ post
  where adjFp = maybe [] (\x -> [x]) $ adjustFp fr

fseq :: [Stm] -> Stm
fseq []     = ExpS $ Const 0
fseq [s]    = s
fseq (x:xs) = Seq x (fseq xs)

adjustFp :: Frame -> Maybe Stm
adjustFp fr 
  | infr == 0 = Nothing
  | otherwise = Just $ T.Move (Temp sp) (Mem (Binop Minus (Temp sp) (Const infr)))  
  where isInFrame (InFrame _) = True
        isInFrame _         = False
        infr = P.length $ P.filter  isInFrame $ formalsAcc fr

adjustArgs :: [Access] -> [Temp] -> Int -> [Stm]
adjustArgs [] _ _              = []
adjustArgs ((InReg t):as) [] i =
  T.Move (Temp t) (Mem (Binop Plus (Temp fp) (Const $ wSz * i + 2 * wSz))) : adjustArgs as [] (i + 1)
adjustArgs ((InFrame k):as) [] i =
  T.Move (Mem (Binop Plus (Temp fp) (Const k))) 
         (Mem (Binop Plus (Temp fp) (Const $ wSz * i + 2 * wSz))) : adjustArgs as [] (i + 1)
adjustArgs ((InReg t):as) (r:rs) i   = 
  T.Move (Temp t) (Temp r) : adjustArgs as rs i
adjustArgs ((InFrame k):as) (r:rs) i = 
  T.Move (Mem (Binop Plus (Temp fp) (Const $ wSz * k))) (Temp r) : adjustArgs as rs i

allocCallee :: (Monad w, TLGenerator w) => w ([Stm], [Stm]) 
allocCallee = 
  do ts <- mapM (\i -> newTemp) [1..P.length calleesaved]
     let newTs = zip ts [0..P.length calleesaved - 1]
     let m1 = P.map (\(t, i) -> T.Move (Temp t) (Temp $ calleesaved !! i)) newTs 
     let m2 = P.map (\(t, i) -> T.Move (Temp $ calleesaved !! i) (Temp t)) newTs
     return (m1, m2)

procEntryExit2 :: Frame -> [Instr] -> [Instr]
procEntryExit2 fr instrs =
  instrs ++ [Oper{assem = "", src = specialregs ++ calleesaved, 
             dst = [], jump = Nothing}] 

data FrameFunc = FF {prolog :: [Instr], body :: [Instr], epilogue :: [Instr]}
  deriving Show

mkProlog :: Frame -> [Instr]
mkProlog fr = 
  [A.Oper{assem = "pushq `s0\n", dst = [], src = [fp], jump = Nothing},
   A.Move{assem = "movq `s0, `d0\n", dst = [fp], src = [sp]},
   A.Oper{assem = "addq $-8, `d0\n", dst = [sp], src = [], jump = Nothing}]

mkEpil :: Frame -> [Instr]
mkEpil fr =
  let stackEpil = [A.Move{assem = "movq `s0, `d0\n", dst = [sp], src = [fp]},
                   A.Oper{assem = "popq `d0\n", dst = [fp], src = [], jump = Nothing},
                   A.Oper{assem = "ret\n", dst = [], src = [], jump = Nothing}]
  in case name fr of
       "tigermain" -> 
         [A.Oper{assem = "jmp `j0\n", dst = [], src = [], jump = Just ["final"]}]
       _ -> stackEpil


procEntryExit3 :: Frame -> [Instr] -> FrameFunc
procEntryExit3 fr bd = 
  FF{prolog =  head bd : mkProlog fr,
     body = tail bd,
     epilogue = mkEpil fr}
