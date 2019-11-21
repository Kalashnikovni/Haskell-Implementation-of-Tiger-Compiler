module TigerErrores where

import TigerSymbol
import TigerTips

-- | La semántica de Demon es que espera que después de 'derror' la computación
-- __no continúe__.
-- 'derror sym >>= _ = derror sym'
class Demon w where
  -- Métodos básicos.
  derror :: Symbol -> w a
  adder :: w a -> Symbol -> w a
  -- Funciones adicionales
  internal :: Symbol -> w a
  internal = derror . addStr "Internal: "
  notfound :: Symbol -> w a
  notfound  = derror . addStr "Not found: "

derrorAux :: Demon w => String -> w a
derrorAux = derror . pack

internalAux :: Demon w => String -> w a
internalAux = internal . pack

-- Agregamos la posicion en el programa donde esta el error
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = adder t (pack $ show p ++ "\n")

errorTiposMsg :: (Demon w, Show p) => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 = flip addpos p
    $ flip adder (pack $ msg ++ "\n")
    $ errorTipos t1 t2

-- | Función /linda/ para mostrar un error de tipos.
errorTipos :: Demon w => Tipo -> Tipo -> w a
errorTipos t1 t2 = derror $ pack $ "Error de tipos."
                   -- Notar que acá se van a mostrar de forma re crota.
                   ++ " Tipo *" ++ show t1
                   ++ "* es distinto a *"
                   ++ show t2 ++ "*."

