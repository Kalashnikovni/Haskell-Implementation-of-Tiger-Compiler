module TigerMunch where

import TigerAssem as A
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

import Debug.Trace

format :: (ATemp -> String) -> Instr -> String
format f (Oper a dst src j) = formatAux f a dst src j
format f (ILabel a lab) = a
format f (A.Move a dst src) = formatAux f a dst src Nothing 

formatAux :: (ATemp -> String) -> String -> [ATemp] -> [ATemp] -> Maybe [ALabel] -> String
formatAux _ [] _ _ _ = []
formatAux f ('`':'s':rest) dst src jmp =
  let (digs, nodigs) = span isDigit rest
  in f (src !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp
formatAux f ('`':'d':rest) dst src jmp =
  let (digs, nodigs) = span isDigit rest
  in f (dst !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp 
formatAux f ('`':'j':rest) dst src jmp 
  | isNothing jmp = error "Wat?"
  | otherwise = let (digs, nodigs) = span isDigit rest
                in (unpack $ (fromJust jmp) !! (read digs :: Int)) ++ formatAux f nodigs dst src jmp
formatAux f (r:rest) dst src jmp = r : (formatAux f rest dst src jmp)

-- Esta es la función que va a tomar como primer argumento format
opmakestring :: ATemp -> String
opmakestring t = '%' : unpack t

codeGen :: (Assembler w) => Stm -> w [Instr]
codeGen stm = 
  do stms <- canonM stm
     mapM_ munchStm stms
     instrs <- getInstrs
     return $ reverse instrs

codeGenStms :: (Assembler w) => [Stm] -> w [[Instr]]
codeGenStms []     = return []
codeGenStms (s:ss) =
  do instrs <- codeGen s
     newSS <- codeGenStms ss
     return $ instrs : newSS

canonTest :: (Assembler w) => Stm -> w [Stm]
canonTest stm = canonM stm

munchArgs :: (Assembler w) => Int -> [Exp] -> w [ATemp]
munchArgs _ []       = return []
munchArgs i (a:args) 
  | i < argsRegsCount = 
    do a' <- munchExp a
       let reg = argregs !! i
       emit Oper{assem = "movq `s0, `d0\n",
                 dst = [reg], src = [a'], jump = Nothing}
       args' <- munchArgs (i + 1) args
       return $ reg:args'
  | otherwise =
    do a' <- munchExp a
       emit Oper{assem = "pushq `s0\n",
                 dst = [], src = [a'], jump = Nothing}
       munchArgs (i + 1) args

result :: (Assembler w) => (ATemp -> w ()) -> w ATemp
result gen = 
  do t <- newTemp
     gen t
     return t 

-- Auxiliary to write less
moving :: (Assembler w) => ATemp -> ATemp -> w ()
moving t1 t2
  | t1 == t2  = return ()
  | otherwise = emit A.Move{assem = "movq `s0, `d0\n", dst = [t2], src = [t1]}

munchExp :: (Assembler w) => Exp -> w ATemp
munchExp (Const i) = 
  result (\r -> emit Oper{assem = "movq $" ++ show i ++ ", `d0\n",
                          dst = [r], src = [], jump = Nothing})
munchExp (Name n) = 
  result (\r -> emit Oper{assem = "movq $" ++ unpack n ++ ", `d0\n",
                          dst = [r], src = [], jump = Nothing})  
munchExp (Temp t) = return t
munchExp (Binop Plus (Const i) e2) = 
  do e2' <- munchExp e2
     result (\r -> do emit Oper{assem = "movq $" ++ show i ++ ", `d0\n",
                                dst = [r], src = [], jump = Nothing}
                      emit Oper{assem = "addq `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Plus e1 (Const i)) = 
  do e1' <- munchExp e1
     result (\r -> do emit Oper{assem = "movq $" ++ show i ++ ", `d0\n",
                                  dst = [r], src = [], jump = Nothing}
                      emit Oper{assem = "addq `s0, `d0\n",
                                dst = [r], src = [e1'], jump = Nothing})
munchExp (Binop Plus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "addq `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Minus e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "subq `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Mul e1 e2) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "imul `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Div e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r0], src = [e1']}
                      emit A.Oper{assem = "xorq `s0, `d0\n",
                                  dst = [a2], src = [a2], jump = Nothing}
                      emit A.Oper{assem = "idiv `s0\n",
                                  dst = [], src = [e2'], jump = Nothing}
                      emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [r0]})
munchExp (Binop And (Const i) e2) 
  | i == 0    = result (\r -> emit Oper{assem = "movq $0, `d0\n",
                                        dst = [r], src = [], jump = Nothing}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit A.Move{assem = "movq `s0, `d0\n",
                                 dst = [r], src = [e2']}) 
munchExp (Binop And e1 (Const i)) 
  | i == 0    = result (\r -> emit Oper{assem = "movq $0, `d0\n",
                                        dst = [r], src = [], jump = Nothing}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit A.Move{assem = "movq `s0, `d0\n",
                                 dst = [r], src = [e1']}) 
munchExp (Binop And e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "and `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop Or (Const i) e2) 
  | i /= 0    = result (\r -> emit Oper{assem = "movq $1, `d0\n",
                                        dst = [r], src = [], jump = Nothing}) 
  | otherwise = 
    do e2' <- munchExp e2
       result (\r -> emit A.Move{assem = "movq `s0, `d0\n",
                                 dst = [r], src = [e2']}) 
munchExp (Binop Or e1 (Const i)) 
  | i /= 0    = result (\r -> emit Oper{assem = "movq $0, `d0\n",
                                        dst = [r], src = [], jump = Nothing}) 
  | otherwise = 
    do e1' <- munchExp e1
       result (\r -> emit A.Move{assem = "movq `s0, `d0\n",
                                 dst = [r], src = [e1']}) 
munchExp (Binop Or e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "or `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop XOr e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "xor `s0, `d0\n",
                                dst = [r], src = [e2'], jump = Nothing})
munchExp (Binop LShift (Const i) e2) =
  do e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e2']}
                      emit Oper{assem = "shl $" ++ show i ++ ", `d0\n",
                                dst = [r], src = [], jump = Nothing})
{-
munchExp (Binop LShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e1']}
                      emit Oper{assem = "shl `d0, `s0, `s1\n",
                                dst = [r], src = [e1', e2'], jump = Nothing})
-}
munchExp (Binop RShift (Const i) e2) =
  do e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e2']}
                      emit Oper{assem = "shr $" ++ show i ++ ", `d0\n",
                                dst = [r], src = [], jump = Nothing})
{-
munchExp (Binop RShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srlv `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
-}
munchExp (Binop ARShift (Const i) e2) =
  do e2' <- munchExp e2
     result (\r -> do emit A.Move{assem = "movq `s0, `d0\n",
                                  dst = [r], src = [e2']}
                      emit Oper{assem = "sar $" ++ show i ++ ", `d0\n",
                                dst = [r], src = [], jump = Nothing})
munchExp (Binop ARShift e1 e2) =
  do e1' <- munchExp e1
     e2' <- munchExp e2
     result (\r -> emit Oper{assem = "srav `d0, `s0, `s1\n",
                             dst = [r], src = [e1', e2'], jump = Nothing})
munchExp (Mem (Binop Plus (Const i) e2)) =
  do e2' <- munchExp e2
     result (\r -> emit Oper{assem = "movq " ++ show i ++ "(`s0), `d0\n",
                             dst = [r], src = [e2'], jump = Nothing})
munchExp (Mem (Binop Plus e1 (Const i))) =
  do e1' <- munchExp e1
     result (\r -> emit Oper{assem = "movq " ++ show i ++ "(`s0), `d0\n",
                             dst = [r], src = [e1'], jump = Nothing})
munchExp (Mem (Const i)) =
  result (\r -> emit Oper{assem = "movq $" ++ show i ++ ", `d0\n",
                          dst = [r], src = [], jump = Nothing})
munchExp (Mem e) =
  do e' <- munchExp e
     result (\r -> emit Oper{assem = "movq 0(`s0), `d0\n",
                             dst = [r], src = [e'], jump = Nothing})
munchExp e = internalAux $ "Revisar etapas, hasta selección de instrucciones -- TigerMunch 1"
  
munchStm :: (Assembler w) => Stm -> w ()
munchStm (Tree.Move (Temp t) (Call e@(Name l) args)) =
  do args' <- munchArgs 0 args
     emit Oper{assem = "call " ++ unpack l ++ "\n",
               dst = calldefs, src = args', jump = Just [l]}
     moving rv t
munchStm (Tree.Move (Temp t) (Const i)) =
  emit Oper{assem = "movq $" ++ show i ++ ", `d0\n",
            dst = [t], src = [], jump = Nothing}
munchStm (Tree.Move (Temp t) e2) =
  do e2' <- munchExp e2  
     moving e2' t
munchStm (Tree.Move (Mem e1) e2) =
  do e1' <- munchExp e1 -- movq 16, newr1; addq rbp, new1 
     e2' <- munchExp e2 -- movq 0, newr2
     emit Oper{assem = "movq `s0, (`d0)\n", -- movq newr2, (new1) 
               dst = [e1'], src = [e2'], jump = Nothing}
munchStm (Jump (Name n) Nothing)  = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 2"
munchStm (Jump (Name n) (Just l)) =
  emit Oper{assem = "jmp `j0\n", 
            dst = [], src = [], jump = Just [l]}
munchStm (Jump e1 Nothing) = 
  do e1' <- munchExp e1
     emit Oper{assem = "jmp `s0\n", 
               dst = [], src = [e1'], jump = Nothing} 
munchStm (Jump _ _) = internalAux "Revisar etapas, hasta selección de instrucciones -- TigerMunch 3"
munchStm (CJump Tree.EQ e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "je `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump Tree.NE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jne `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump Tree.LT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jl `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump Tree.GT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jg `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump LE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jle `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump GE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jge `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump ULT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "jb `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump UGT e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "ja `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump ULE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "je `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jb `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (CJump UGE e1 e2 lt lf) = 
  do e1' <- munchExp e1
     e2' <- munchExp e2
     emit Oper{assem = "cmp `s0, `s1\n",
               dst = [], src = [e1', e2'], jump = Nothing}
     emit Oper{assem = "je `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "ja `j0\n",
               dst = [], src = [], jump = Just [lt]}
     emit Oper{assem = "jmp `j0\n",
               dst = [], src = [], jump = Just[lf]}
munchStm (Label l) =
  emit ILabel{assem = unpack l ++ ":\n", lab = l}
munchStm (ExpS (Call e@(Name l) args)) =
  do args' <- munchArgs 0 args
     emit Oper{assem = "call " ++ unpack l ++ "\n",
               dst = calldefs, src = args', jump = Just [l]}
     return ()
munchStm (ExpS e) =
  do munchExp e
     return ()
munchStm s = internalAux $ "Revisar etapas, hasta selección de instrucciones -- TigerMunch 4" ++
                           show s 
  

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\ --
-- Tipos de estados -------------------------------------------------------------------------------------- --
-- /////////////////////////////////////////////////////////////////////////////////////////////////////// --

data AssemEstado = AEst {instrs :: [Instr], tam :: TAM}
initAEstado = AEst {instrs = [], tam = firstTank}
type AssemMonada = ExceptT Symbol (StateT AssemEstado StGen)

runMonada3 :: AssemMonada [Instr] -> StGen (Either Symbol [Instr])
runMonada3 = flip evalStateT initAEstado . runExceptT

runMonada4 :: AssemMonada [([Stm], Frame)] -> StGen (Either Symbol [([Stm], Frame)])
runMonada4 = flip evalStateT initAEstado . runExceptT

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

