# Haskell-Implementation-of-Tiger-Compiler

Proyecto final de la materia "Compiladores" del IV año de LCC - FCEIA, Rosario, Argentina.

# Testing

Para testear usamos la herramienta *stack* que buildea proyectos pasándole un archivito
de configuración. Si queremos cargar los módulos en ghci:

```
stack ghci
```
Después fuimos implementando algunas test suite para ir chequeando lo que pudimos
avanzar en las distintas etapas. Para correr todas ellas:

```
stack test
```

En caso de querer centrarnos en una única test suite escribimos:

```
stack test :NameTestSuite
```

donde NameTestSuite es el nombre de la test suite que queremos usar.

Ahora, para compilar todo el proyecto en sí, tenemos app/TigerMain.hs. Para construir
el proyecto se debe ejecutar:

```
stack setup
stack build
stack exec -- HaskTiger filenamewithpath -opt
```

donde filenamewithpath es el nombre del archivo, desde la ruta actual; y opt es una
de las opciones listadas en TigerMain, por ejemplo: -a (muestra el AST).

Pd: stack funcionó solo instalando usando el siguiente comando:

```
curl -sSL https://get.haskellstack.org/ | sh
```
A continuación listamos algunas decisiones de diseño del compilador, que deben ser
tenidas en cuenta si se desea analizar la construcción que hicimos del mismo.

# Decisiones
- No hacemos chequeos en las cotas de los loops (si lo < hi).
- Por ahora todas las variables escapan.
- El nivel inicial es 0. La funcion "mas anidada" tiene el mayor numero.
  Cuando subimos de nivel aumenta el contador, y disminuye cuando bajamos de nivel.
- Para la lista de fragmentos, agregamos los nuevos fragmentos al principio.

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
- [X] Ver si los tipos de errores (internal, etc.) de TigerSeman están bien usados.
- [X] Revisar que nunca usamos insertVRO ¿No tendríamos que hacerlo?
- [X] simpleVar en TigerTrans
- [X] Codigo intermedio para la variable fresca de los for.
- [X] Alloc de variables en declaracion de funciones.
- [X] Dar instancia de Monada para MemM, sino nos arma lio con transExp
- [X] Revisar transDec, que ahora toma otro argumento mas (segundo elemento de la tupla).
      Cambió el tipado de transExp, así que tuvimos que acomodar transDecs.
- [ ] Revisar TigerTrans.ifThenElseExp optimizaciones. 
- [ ] Revisar 2° etapa: separación de código; no podemos usar las expresiones directo!
- [ ] Revisar 2° etapa, para recursive-lets.tig nos queda loopeando forever.
- [X] 2° etapa. Creemos que funciona, peeeero, siempre puede haber macana por ahí, y faltaría
      ocuparse de las optimizaciones.
- [ ] Mejorar TigerMain, TigerInterp (que no vimos nah de nah), y test suites (que más o menos
      estan, pero revisar por las dudas).

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
- [X] ¿Por qué en el código de la carpeta para genSl usa 2 * wSz? ¿Y por qué siempre suma este valor?
      ¿El static link siempre está en la misma ubicación dentro del frame de una función?
      (Hoja 35 de carpeta de Denu).
      Rta: el static link está siempre en la misma posición del frame, por eso sumamos
      siempre la misma constante.
- [X] ¿La idea de fpPrevLvl es que vaya cambiando cada vez que entramos en un nuevo nivel?
      O sea, nos dice que tenemos que ajustarlo bien ¿Quizás es por la arquitectura elegida?
      Rta: no, fpPrevLvl va a ser constante; tiene que ver con la arquitectura. Otro dato:
      fpPrevLvl está por si tenemos funciones que se llaman en el mismo nivel; con fpPrevLvl
      podemos acceder al frame del nivel anterior, y no solo al mismo nivel del llamante
      y el llamado.
- [X] En TigerFrame.exp chequea si c == 0 y ahi tira error ¿No deberia ser al revés?
      Rta: sí, c == 0 debería dar error.
- [X] ¿Por que TigerTree.Jump no toma la lista de labels? (Página 150 del libro).
      Rta: porque en la mayoría de las arquitecturas solo saltamos a un label,
      no a una lista de posibilidades de labels, así que tiene más sentido hacerlo
      así. Igual si la arquitectura lo soporta, podríamos cambiar esa estructura.
- [X] ¿Por que en la carpeta en TigerTrans.forExp metemos a hi en un tmp?
      Onda ¿No le podemos hacer directamente unEx, y usar el resultado del unEx
      para los CJump?
      Rta: en un assembler posta no usamos las expresiones así de una, sino que los metemos
      en registro, así que es mucho mejor mandarlos a registro y después operar con los
      registros.
- [X] Para seguir static links ¿Cómo hacemos? ¿Nos alcanza con que el frame tenga
      [Escapa] en vez de [Access]? (para TigerTrans.callExp, que hay que insertar el sl).
      Rta: sí, nos alcanza, porque el sl siempre está en el mismo offset dentro de un frame.
- [X] ¿Cómo vamos haciendo la parte de generación de código intermedio para funciones?
      Rta: por ahora usamos una implementación por default, hasta que en la tercera etapa
      terminemos de definir cosas de la arquitectura que afectan a esto de las funciones.
- [X] ¿Por qué no generamos codigo intermedio para las declaraciones de tipo?
      Rta: porque en assembler no tenemos tipos, onda son anotaciones en el lenguaje
      a compilar, que nos permiten ciertos chequeos, pero no es que tengamos que
      ejecutar nada para que se ejecuten las sentencias del programa. 
- [X] ¿Qué es la parte de fragments? (Página 169 del libro).
      Rta: es para separar código ejecutable, de aquel que no lo es.
- [X] ¿Tenemos que diferenciar al generar codigo intermedio para las operaciones binarias?
      (Onda en TigerTrans tenemos binOpIntRelExp y binOpIntExp, calculamos que es para optimizar)
      Rta: lo que pasa es que binOpIntRelExp va a devolver un Cx, mientras que binOpInExp
      devuelve una Ex.
- [X] ¿El nivel más externo de un programa debería tener su fragmento?
      Rta: sí, por eso wrappeamos nuestro código fuente en una LetExp que llamamos
      _tigermain, así se genera el fragmento para el nivel más externo. Onda como
      en C, todo arranca desde el main.
- [X] ¿Por qué TigerTrans.seqExp tira error si el ultimo comando es condicional?
      Rta: porque estaba mal en el template, ya corregido.
- [X] ¿Por qué en TigerTrans.seqExp Tincho no nos dio el caso de Cx?
      Rta: contestado arriba.
- [ ] Las llamadas externas ¿Deberían tomar en la lista las expresiones directamente
      o antes deberíamos guardar las expresiones en temporarios?
- [ ] ¿Por que en el codigo de la carpeta para simpleVar devuelve el temp1?
- [ ] Cuando usamos canonM ¿Los "statemencitos" resultantes deben usar el mismo
      frame que antes de canonizar?
- [ ] ¿Qué representan los TigerTree.Name? ¿Que diferencia tiene con TigerTree.Label?
      ¿Qué código deberíamos emitir?
- [ ] ¿Está bien lo que hicimos para TigerAssem.munchExp(Binop And (Const i) e2)?
- [ ] ¿Memoria vs stack? Aparece en TigerFrame.

