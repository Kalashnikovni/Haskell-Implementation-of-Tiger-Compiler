module TigerSres where

import TigerTips
import TigerTemp
import TigerUnique
import TigerTrans
import TigerFrame

data Externa = Runtime | Propia
    deriving Show

type FunEntry = (Level, Label, [Tipo], Tipo, Bool)

-- (Tipo, acceso, nivel de la variable)
type ValEntry = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
