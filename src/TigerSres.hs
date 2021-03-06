module TigerSres where

import TigerTips
import TigerTemp
import TigerUnique
import TigerTrans
import TigerFrame

data Externa = Runtime | Propia
    deriving Show

--(Level de declaracion, nombre, tipo de args, tipo de retorno, externa o no)
type FunEntry = (Level, Label, [Tipo], Tipo, TigerSres.Externa)

-- (Tipo, acceso, nivel de la variable)
type ValEntry = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
