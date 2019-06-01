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
- [ ] Chequear en transTy qué Posicion debería tener cada campo ¿Es algo de la segunda o tercer etapa?
- [ ] Terminar transExp.
- [ ] Completar TigerSeman (transDec, instancias de mónada).
- [ ] Opcional: ver lo de pretty-printing.
