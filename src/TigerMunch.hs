module TigerMunch where

import TigerAssem
import TigerCanon
import TigerErrores
import TigerFrame
import TigerSymbol
import TigerTemp
import TigerTree as Tree
import TigerUnique

import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Char
import Data.List
import Data.Maybe
import Data.Map as Map

format :: (ATemp -> String) -> Instr -> String
format f (Oper a dst src j) = formatAux f a dst src j
format f (ILabel a lab) = a
format f (TigerAssem.Move a dst src) = formatAux f a dst src Nothing 

formatAux :: (ATemp -> String) -> String -> [ATemp] -> [ATemp] -> Maybe [ALabel] -> String
formatAux _ [] _ _ _ = []
formatAux f ('`':'s':rest) dst src jmp =
  let (digs, nodigs) = span isDigit rest
  in f (src !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp
formatAux f ('`':'d':rest) dst src jmp =
  let (digs, nodigs) = span isDigit rest
  in f (dst !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp 
formatAux f ('`':'j':rest) dst src jmp 
  | isNothing jmp = error "WAT"
  | otherwise = let (digs, nodigs) = span isDigit rest
                in (unpack $ (fromJust jmp) !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp
formatAux f (r:rest) dst src jmp = r : (formatAux f rest dst src jmp)

-- Esta es la función que va a tomar como primer argumento format
opmakestring :: ATemp -> String
opmakestring t = '$' : unpack t

codeGen :: (Assembler w) => Frame -> Stm -> w [Instr]
codeGen fr stm = 
  do stms <- canonM stm
     mapM_ munchStm stms
     instrs <- getInstrs
     return $ reverse instrs

munchArgs :: (Assembler w) => Int -> [Exp] -> w [ATemp]
munchArgs _ []       = return []
munchArgs i (a:args) 
  | i == argsRegsCount =
    do a' <- munchExp a
       emit Oper{assem = "sub `d0, `s0, 4 \n sw `s1, 0(`s0)\n",
                 dst = [sp], src = [sp, a'], jump = Nothing}
       munchArgs (i + 1) args
  | otherwise = 
    do a' <- munchExp a
       let reg = argregs !! i
       emit Oper{assem = "move `d0, `s0\n",
                 dst = [reg], src = [a'], jump = Nothing}
       args' <- munchArgs (i + 1) args
       return $ a':args'

result :: (Assembler w) => (ATemp -> w ()) -> w ATemp
result gen = 
  do t <- newTemp
     gen t
     return t 

munchExp :: (Assembler w) => Exp -> w ATemp
munchExp (Const i) = 
  result (\r -> emit Oper{assem = "addi `d0, `s0, " ++ show i ++ "\n",
                          dst = [r], src = [zero], jump = Nothing})
--munchExp (Name n) = result  
munchExp (Temp t) = return t
munchExp (Binop Plus (Const i) e2) = 
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "addi `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Plus e1 (Const i)) = 
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "addi `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop Plus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "add `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop Minus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "sub `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop Mul e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "mult `s0, `s1\n",
                             dst = [lo, hi], src = [e1', e2'], jump = Nothing})
munchExp (Binop Div e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "div `s0, `s1\n",
                             dst = [lo, hi], src = [e1', e2'], jump = Nothing})
munchExp (Binop And (Const i) e2) 
  | i == 0    = result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                                   dst = [r], src = [zero]}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                          dst = [r], src = [e2']}) 
munchExp (Binop And e1 (Const i)) 
  | i == 0    = result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                                   dst = [r], src = [zero]}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                          dst = [r], src = [e1']}) 
munchExp (Binop And e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "and `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop Or (Const i) e2) 
  | i /= 0    = result (\r -> emit TigerAssem.Move{assem = "move `d0, 1\n",
                                                   dst = [r], src = []}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                          dst = [r], src = [e2']}) 
munchExp (Binop Or e1 (Const i)) 
  | i /= 0    = result (\r -> emit TigerAssem.Move{assem = "move `d0, 1\n",
                                                   dst = [r], src = []}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit TigerAssem.Move{assem = "move `d0, `s0\n",
                                          dst = [r], src = [e1']}) 
munchExp (Binop Or e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "or `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop XOr (Const i) e2) =
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "xori `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop XOr e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "xori `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop XOr e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "xor `d0, `s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop LShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "sll `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop LShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "sllv `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop RShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "srl `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop RShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srlv `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Binop ARShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "sra `d0, `s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop ARShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srav `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Mem (Binop Plus (Const i) e2)) =
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "lw `d0, " ++ show i ++ "(`s0)\n",
                             dst = [r], src = [e2'], jump = Nothing})
munchExp (Mem (Binop Plus e1 (Const i))) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "lw `d0, " ++ show i ++ "(`s0)\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Mem (Const i)) =
  result (\r -> emit Oper{assem = "lw `d0, " ++ show i ++ "(`s0)\n",
                          dst = [r], src = [zero], jump = Nothing})
munchExp (Mem e) =
  do e' <- munchExp e
     result (\r -> emit Oper{assem = "lw `d0, 0(`s0)\n",
                            dst = [r], src = [e'], jump = Nothing})
munchExp (Eseq stm e) = 
  do munchStm stm
     munchExp e
munchExp (Call e@(Name l) args) =
  do args' <- munchArgs 0 args
     emit Oper{assem = "jr `s0\n",
               dst = [], src = [ra], jump = Nothing}
     e' <- munchExp e
     emit Oper {assem = "jal `j0\n",
                dst = calldefs, src = e' : args', jump = Nothing}
munchExp _ = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 1"
  
munchStm :: (Assembler w) => Stm -> w ()
munchStm (Tree.Move (Temp t) e2) =
  do e2' <- munchExp e2  
     emit TigerAssem.Move{assem = "move `d0, `s0\n", dst = [t], src = [e2']}
munchStm (Tree.Move (Mem (Binop Plus (Const i) e1)) e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw `s1, " ++ show i ++ "(`s0)\n", dst = [], src = [e1', e2'], jump = Nothing} 
munchStm (Tree.Move (Mem (Binop Plus e1 (Const i))) e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw `s1, " ++ show i ++ "(`s0)\n", dst = [], src = [e1', e2'], jump = Nothing}
munchStm (Tree.Move (Mem e1) (Mem e2)) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw `s0, 0(`s1)\n", dst = [], src = [e1', e2'], jump = Nothing}
munchStm (Tree.Move (Mem e1) e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw `s1, 0(`s0)\n", dst = [], src = [e1', e2'], jump = Nothing}
munchStm (Jump (Name n) Nothing)  = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 2"
munchStm (Jump (Name n) (Just l)) =
  emit Oper{assem = "j `j0\n", dst = [], src = [], jump = Just [l]}
munchStm (Jump e1 Nothing) = 
  do e1' <- munchExp e1
     emit Oper{assem = "jr `j0\n", dst = [], src = [e1'], jump = Nothing} 
munchStm (Jump _ _) = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 3"
munchStm (CJump Tree.EQ e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "beq `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump Tree.NE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "beq `s0, `s1, " ++ unpack lf ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump Tree.LT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "blt `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump Tree.GT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgt `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump LE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "ble `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump GE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bge `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump ULT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bltu `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump UGT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgtu `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump ULE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bleu `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (CJump UGE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgeu `s0, `s1, " ++ unpack lt ++ "\nj `j0\n",
               dst = [], src = [e1', e2'], jump = Just [lt, lf]}
munchStm (Seq s1 s2) =
  do munchStm s1
     munchStm s2
     return ()
munchStm (Label l) =
  emit ILabel{assem = unpack l ++ ":\n", lab = l}
munchStm (ExpS (Call e@(Name l) args)) =
  do args' <- munchArgs 0 args
     emit Oper{assem = "jr `s0\n",
               dst = [], src = [ra], jump = Nothing}
     e' <- munchExp e
     emit Oper {assem = "jal `j0\n",
                dst = calldefs, src = e' : args', jump = Nothing}
munchStm _ = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 4"
  

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data AssemEstado = AEst {instrs :: [Instr], tam :: TAM}
initAEstado = AEst {instrs = [], tam = firstTank}
type AssemMonada = ExceptT Symbol (StateT AssemEstado StGen)

runMonada3 :: AssemMonada [[Instr]] -> StGen (Either Symbol [[Instr]])
runMonada3 = flip evalStateT initAEstado . runExceptT

class (Demon w, Monad w, TLGenerator w, Trackable w) => Assembler w where
  emit :: Instr -> w ()
  getInstrs :: w [Instr]

instance Demon AssemMonada where
  derror      =  throwE . pack . (++ "\n") . unpack 
  adder w msg = withExceptT (\e -> addStr (unpack msg) e) w 

instance Trackable AssemMonada where
  enterBlock' l b =
    do st <- get
       put st{tam = Map.insert l b $ tam st} 
  getBlock l = gets $ Map.lookup l . tam 

instance Assembler AssemMonada where
  emit instr = 
    do st <- get
       put st{instrs = instr : (instrs st)}
       return ()
  getInstrs =
    do st <- get
       return $ instrs st

