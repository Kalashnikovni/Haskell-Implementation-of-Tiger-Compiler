-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- ¿Por qué usamos este tipo de datos en vez de Strings? Bueno,
-- sencillamente por una cuestión de rendimiento, ya que con Symbol
-- las comparaciones (y otras operaciones) son más eficientes
-- ////////////////////////////////////////////////////////////////

module TigerSymbol (Symbol, pack, unpack, addStr, append, appends, T.length, T.empty) where

import Data.Text as T
import Prelude   as P

-- | Symbol es como representamos las cadenas a lo largo del compilador...
type Symbol = T.Text

addStr :: String -> Symbol -> Symbol
addStr str = pack . (++) str . unpack

appends :: [Symbol] -> Symbol
appends = P.foldr1 append
