-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- Este módulo se separa de TigerTips porque TigerSres define tipos
-- para los entornos que se usan en el análisis semántico, mientras
-- que TigerTips define los tipos de Tiger.
-- ////////////////////////////////////////////////////////////////

module TigerSres where

import TigerTips
import TigerTemp
import TigerUnique
-- import TigerTrans
-- import TigerFrame

-- | 'Externa' representa la idea si una función pertenece al /runtime/ o no.
data Externa = Runtime | Propia
    deriving Show

type FunEntry = (Unique, Label, [Tipo], Tipo, Externa)
-- type FunEntry = (Level, Label, [Tipo], Tipo, Bool)

type ValEntry = Tipo -- Entrega2 -> = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
