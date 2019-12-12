module TigerAssem where

import TigerTemp

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


