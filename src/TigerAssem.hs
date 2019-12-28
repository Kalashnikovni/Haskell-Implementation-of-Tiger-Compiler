module TigerAssem where

import TigerFrame
import TigerTemp
import TigerTree

type AReg   = String
type ATemp  = TigerTemp.Temp
type ALabel = TigerTemp.Label

data Instr = Oper {assem :: String,
                   dst :: [ATemp], -- MIRAR MIPS
                   src :: [ATemp],
                   jump :: [Maybe ALabel]}
             | ILabel {assem :: String,
                       lab :: ALabel}
             | Move {assem :: String,
                     dst :: ATemp,
                     src :: ATemp}
          deriving Show

--format :: (ATemp -> String) -> Instr -> String
--format f (Oper o) = 

--codeGen :: Frame -> Stm -> w [Instr]

munchArgs :: Int -> [Exp] -> w [ATemp]
munchArgs _ []       = return []
munchArgs i (a:args) =
  | i == argsRegCount =
    do a' <- munchExp a
       emit Oper{assem = "sub $sp, $sp, 4 \n sw $s0, 0($sp)\n",
                 dst = [], src = [a'], jump = []}
       munchArgs (i + 1) args
  | otherwise = 
    do a' <- munchExp a
       let reg = argregs !! i
       emit Oper{assem = "move $" ++ unpack reg ++ " , $s0\n",
                 dst = [reg], src = [a'], jump = []}
       args' <- munchArgs (i + 1) args
       return $ a':args'

result :: (Assembler w) => (ATemp -> w ()) -> w ATemp
result gen = 
  do t <- newTemp
     gen t
     return t 

munchExp :: (Assembler w) => Exp -> ATemp
munchExp (Const i) = 
  result (\r -> emit Oper{assem = "addi $d0, $zero, " ++ show i ++ "\n",
                          dst = [r], src = [], jump = []})
--munchExp (Name n) = result  
munchExp (ATemp t) = t
munchExp (Binop Plus (Const i) e2) = 
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "addi $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e2'], jump = []})
munchExp (Binop Plus e1 (Const i)) = 
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "addi $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Binop Plus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "add $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop Minus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "sub $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop Mul e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "mult $s0, $s1\n",
                             dst = [lo, hi], src = [e1', e2'], jump = []})
munchExp (Binop Div e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "div $s0, $s1\n",
                             dst = [lo, hi], src = [e1', e2'], jump = []})
munchExp (Binop And (Const i) e2) 
  | i == 0    = result (\r -> emit Move{assem = "move $d0, $zero\n",
                                        dst = [r], src = [], jump = []}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit Move{assem = "move $d0, $s0\n",
                               dst = [r], src = [e2'], jump = []}) 
munchExp (Binop And e1 (Const i)) 
  | i == 0    = result (\r -> emit Move{assem = "move $d0, $zero\n",
                                        dst = [r], src = [], jump = []}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit Move{assem = "move $d0, $s0\n",
                               dst = [r], src = [e1'], jump = []}) 
munchExp (Binop And e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "and $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop Or (Const i) e2) 
  | i /= 0    = result (\r -> emit Move{assem = "move $d0, 1\n",
                                        dst = [r], src = [], jump = []}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit Move{assem = "move $d0, $s0\n",
                               dst = [r], src = [e2'], jump = []}) 
munchExp (Binop Or e1 (Const i)) 
  | i /= 0    = result (\r -> emit Move{assem = "move $d0, 1\n",
                                        dst = [r], src = [], jump = []}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit Move{assem = "move $d0, $s0\n",
                               dst = [r], src = [e1'], jump = []}) 
munchExp (Binop Or e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "or $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop XOr (Const i) e2) =
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "xori $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e2'], jump = []})
munchExp (Binop XOr e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "xori $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Binop XOr e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "xor $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop LShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "sll $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Binop LShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "sllv $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop RShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "srl $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Binop RShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srlv $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Binop ARShift e1 (Const i)) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "sra $d0, $s0, " ++ show i ++ "\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Binop ARShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srav $d0, $s0, $s1\n",
                             dst = [r], src = [e1', e2'], jump = []})
munchExp (Mem (Binop Plus (Const i) e2)) =
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "lw $d0, " ++ show i ++ "($s0)\n",
                             dst = [r], src = [e2'], jump = []})
munchExp (Mem (Binop Plus e1 (Const i))) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "lw $d0, " ++ show i ++ "($s0)\n",
                             dst = [r], src = [e1'], jump = []})
munchExp (Mem (Const i)) =
  result (\r -> emit Oper{assem = "lw $d0, " ++ show i ++ "($zero)\n",
                          dst = [r], src = [], jump = []})
munchExp (Mem e) =
  do e' <- munchExp e
     result (\r -> emit Oper{assem = "lw $d0, 0($s0)\n",
                            dst = [r], src = [e'], jump = []})
munchExp (Eseq stm e) = 
  do munchStm stm
     munchExp e
  
munchStm :: (Assembler w) => Stm -> w ()
munchStm (Move (Temp t) e2) =
  do e2' <- munchExp e2  
     emit Move{assem = "move $d0, $s0\n", dst = t, src = e2'} w
munchStm (Move (Mem $ Binop Plus (Const i) e1) e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw $s1, " ++ show i ++ "($s0)\n", dst = [], src = [e1', e2'], jump = []} 
munchStm (Move (Mem $ Binop Plus e1 (Const i)) e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw $s1, " ++ show i ++ "($s0)\n", dst = [], src = [e1', e2'], jump = []}
munchStm (Move (Mem e1) (Mem e2)) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw $s0, 0($s1)\n", dst = [], src = [e1', e2'], jump = []}
munchStm (Move (Mem e1) e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "sw $s1, 0($s0)\n", dst = [], src [e1', e2'], jump = []}
munchStm (Jump (Name n) Nothing)  = internalAux "Revisar el compilador -- TigerAssem"
munchStm (Jump (Name n) (Just l)) =
  emit Oper{assem = "j " ++ unpack l ++ "\n", dst = [], src = [], jump = [Just l]}
munchStm (Jump e1 Nothing) = 
  do e1' <- munchExp e1
     emit Oper{assem = "jr $s0\n", dst = [], src = [e1'], jump = []} 
munchStm (Jump _ _) _ = internalAux "Revisar el compilador -- TigerAssem"
munchStm (CJump EQ e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "beq $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump NEQ e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "beq $s0, $s1, " ++ unpack lf ++ "\nj " ++ unpack lt ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump LT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "blt $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump GT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgt $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump LE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "ble $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump GE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bge $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump ULT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bltu $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump UGT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgtu $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump ULE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bleu $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (CJump UGE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "bgeu $s0, $s1, " ++ unpack lt ++ "\nj " ++ unpack lf ++ "\n",
               dst = [], src = [e1', e2'], jump = [lt, lf]}
munchStm (Seq s1 s2) =
  do munchStm s1
     munchStm s2
     return ()
munchStm (Label l) =
  emit ILabel{assem = unpack l ++ ":\n", lab = l}
munchStm (ExpS $ CallExp e@(Name l) args) =
  do args' <- munchArgs args
     e' <- munchExp e
     emit Oper{assem = "jr $ra\n",
               dst = [], src = [], jump = []}
     result (\r -> emit Oper {assem = "jal $s0\n",
                              dst = calldefs, src = e' : args', jump = []})
  

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data AssemEstado = AEst {instrs :: [Instr]}

class (Demon w, Monad w, TLGenerator w) => Assembler w where
  emit :: Instr -> w a
  getInstrs :: [Instr]
