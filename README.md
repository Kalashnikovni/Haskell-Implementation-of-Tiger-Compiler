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
- [ ] Completar transExp (caso CallExp)
- [ ] Completar transDecs.
- [ ] Chequeos de breaks bien anidados, y que las definiciones mutuamente recursivas sean a través de  
      arrays o records (¿Puede que se haga en la etapa de parseo?)
- [ ] Chequear en transTy qué Posicion debería tener cada campo ¿Es algo de la segunda o tercer etapa?
- [ ] Opcional: ver lo de pretty-printing.

# Dudas

- [ ] ¿Qué es el argumento de escape de una ForExp?
- [ ] En transExp, para el caso de ForExp ¿No tendríamos que chequear si nv es "fresca"?
- [ ] En transExp, para el caso de ForExp ¿Tenemos que chequear si lo < hi?
