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

-- munchExp :: (Assembler w) => Exp -> w ATemp

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
  

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data AssemEstado = AEst {instrs :: [Instr]}

class (Demon w, Monad w, TLGenerator w) => Assembler w where
  emit :: Instr -> w a
  getInstrs :: [Instr]
