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
- [X] Corregir transDecs, caso TypeDec.
- [X] Chequear que las definiciones mutuamente recursivas sean a través de arrays o records.
- [X] Chequear en transTy qué Posicion debería tener cada campo ¿Es algo de la segunda o tercer etapa?
- [X] Errores significativos en TopSort ¿Qué más podemos dar como error? Rta: cambiar error para que 
      no se corte el testeo.
- [X] Revisar qué pasa con merge.tig que no encuentra readint
- [X] Opcional: ver lo de pretty-printing.
- [ ] Ver si los tipos de errores (internal, etc.) de TigerSeman están bien usados.
- [ ] 2° etapa

# Dudas

- [X] ¿Qué es el argumento de escape de una ForExp? Se refiere si a la variable del contador
      es contador.
- [X] En transExp, para el caso de ForExp ¿No tendríamos que chequear si nv es "fresca"?
      Noup, no vamos a hacer ese chequeo porque somos re heavies y re jodidas.
- [X] En transExp, para el caso de ForExp ¿Tenemos que chequear si lo < hi? Alto mambo!
      Acá nos dejamos llevar por la moda: a llorar a magoya si no te avivaste de lo > hi.
- [X] ¿Qué hacemos cuando se declaran variables y funciones con el mismo nombre? (three-name-spaces2.tig, fun-vs-var.tig)
      Tenemos en cuenta el scope, acá vale la última que se declaró (se re cuelan!).
