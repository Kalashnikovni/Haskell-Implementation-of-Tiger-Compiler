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
- [X] simpleVar en TigerTrans
- [ ] Codigo intermedio para la variable fresca de los for.
- [ ] 2° etapa

# Dudas

- [X] ¿Qué es el argumento de escape de una ForExp?
      Rta: Dice si la variable del contador escapa o no escapa.
- [X] En transExp, para el caso de ForExp ¿No tendríamos que chequear si nv es "fresca"?
      Rta: Noup, no vamos a hacer ese chequeo porque somos re heavies y re jodidas.
- [X] En transExp, para el caso de ForExp ¿Tenemos que chequear si lo < hi? Alto mambo!
      Rta: Acá nos dejamos llevar por la moda: a llorar a magoya si no te avivaste de lo > hi.
- [X] ¿Qué hacemos cuando se declaran variables y funciones con el mismo nombre? (three-name-spaces2.tig, fun-vs-var.tig)
      Rta: Tenemos en cuenta el scope, acá vale la última que se declaró (se re cuelan!).
- [X] ¿Por qué chequeamos que deltaprof > 0 en simpleVar en TigerTrans?
      Rta: Para ver si el nivel donde se usa la variable es mayor a aquel en donde fue declarada.
- [X] ¿Por que cuando llamamos una externalCall no podemos guardar directamente el resultado
      en el temporario de nuestra preferencia? (TigerTrans.recordExp)
      Rta: porque allocRecord devuelve en rv su resultado, y por ahí otra función lo pisa,
      entonces mandamos el resultado a otro temporario para que no perdamos ese valor. 
- [ ] ¿Por qué en el código de la carpeta para genSl usa 2 * wSz? ¿Y por qué siempre suma este valor?
      ¿El static link siempre está en la misma ubicación dentro del frame de una función?
      (Hoja 35 de carpeta de Denu).
- [ ] ¿La idea de fpPrevLvl es que vaya cambiando cada vez que entramos en un nuevo nivel?
      O sea, nos dice que tenemos que ajustarlo bien ¿Quizás es por la arquitectura elegida?
- [ ] En TigerFrame.exp chequea si c == 0 y ahi tira error ¿No deberia ser al revés?
- [ ] ¿Por que en el codigo de la carpeta para simpleVar devuelve el temp1?
- [ ] ¿Por que TigerTree.Jump no toma la lista de labels? (Página 150 del libro).
- [ ] ¿Por que en la carpeta en TigerTrans.forExp metemos a hi en un tmp?
      Onda ¿No le podemos hacer directamente unEx, y usar el resultado del unEx
      para los CJump?
- [ ] Para seguir static links ¿Cómo hacemos? ¿Nos alcanza con que el frame tenga
      [Escapa] en vez de [Access]? (para TigerTrans.callExp, que hay que insertar el sl).
- [ ] ¿Cómo vamos haciendo la parte de generación de código intermedio para funciones?
- [ ] ¿Tenemos que diferenciar al generar codigo intermedio para las operaciones binarias?
      (Onda en TigerTrans tenemos binOpIntRelExp y binOpIntExp, calculamos que es para optimizar)
- [ ] ¿Por qué no generamos codigo intermedio para las declaraciones de tipo?

# Decisiones
- No hacemos chequeos en las cotas de los loops (si lo < hi).
- Por ahora todas las variables escapan.
