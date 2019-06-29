# Haskell-Implementation-of-Tiger-Compiler

Proyecto final de la materia "Compiladores" del IV año de LCC - FCEIA, Rosario, Argentina.

# Testing

Para testear usamos la herramienta *stack* que buildea proyectos pasándole un archivito
de configuración. Para empezar a probar como está el código:

```
stack ghci
```

Pd: stack funcionó solo instalando usando el siguiente comando:

```
curl -sSL https://get.haskellstack.org/ | sh
```
Para chequear tipos podemos hacer:

```
evalState (runSeman exp) 0
```

# TODO

- [X] Ver el caso de transTy en RecordTy.
- [X] Terminar transExp (solo queda el caso de ArrayExp).
- [X] Dar las instancias de mónada (falta testearlas).
- [X] Completar transExp (caso CallExp)
- [ ] Corregir transDecs, caso TypeDec.
- [ ] Chequeos de breaks bien anidados, y que las definiciones mutuamente recursivas sean a través de  
      arrays o records.
- [ ] Chequear en transTy qué Posicion debería tener cada campo ¿Es algo de la segunda o tercer etapa?
- [ ] Errores significativos en TopSort ¿Qué más podemos dar como error?
- [ ] Opcional: ver lo de pretty-printing.

# Dudas

- [ ] ¿Qué es el argumento de escape de una ForExp?
- [ ] En transExp, para el caso de ForExp ¿No tendríamos que chequear si nv es "fresca"?
- [ ] En transExp, para el caso de ForExp ¿Tenemos que chequear si lo < hi?
