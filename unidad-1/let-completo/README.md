# let-completo: el intérprete para la tarea 1

Parte de `let-listas` ya resuelta y trae preparado el terreno para los siete
constructos de la tarea: `not`, `and`, `or`, `xor`, `cond`, `list` y `unpack`.
El enunciado de la tarea dice qué se pide de cada uno, cómo se entrega y cómo
se califica. Aquí va solo lo que trae la carpeta y en qué estado. **Esta
carpeta es la que entregas**, llena.

## Qué trae

Los diez módulos y la carpeta `tests/`. El lexer, el parser, el REPL y las
pruebas ya conocen los siete constructos. Lo demás se reparte así:

| Constructo | Dónde vive | Qué viene hecho | Qué escribes tú |
|---|---|---|---|
| `not` | núcleo | la producción y las reglas, en el enunciado | el `struct` y la cláusula de `interp.rkt` |
| `and` | azúcar | la producción y la regla de transformación, en el enunciado | el `struct` y la cláusula de `desugar.rkt` |
| `or` | núcleo | el `struct` y la cláusula de `interp.rkt` | la producción y las reglas |
| `xor` | azúcar | el `struct` y la cláusula de `desugar.rkt` | la producción y la regla de transformación |
| `cond` | azúcar | la producción y la regla de transformación, en el enunciado, salvo la de `cond end` | el `struct`, la cláusula de `desugar.rkt` y la regla de `cond end` |
| `list` | azúcar | la producción y las reglas de transformación, en el enunciado | el `struct` y la cláusula de `desugar.rkt` |
| `unpack` | núcleo | una descripción informal, en el enunciado | todo: la producción, las reglas, el `struct`, la cláusula de `interp.rkt` y sus auxiliares |

Las marcas `;; --- AQUÍ ... ---` de `ast.rkt`, `interp.rkt` y `desugar.rkt`
dicen dónde va el código. Las producciones que escribes van en el comentario
de `parser.rkt`, donde dice `(escribe la producción de ...)`. La prosa va en
el comentario de bloque que abre `interp.rkt`, que ya trae los encabezados.

También trae, resuelto, todo lo de las hojas de ejercicios: la aritmética, los
booleanos, `+` y `*`, y las listas.

## En qué estado se entrega

Tal como está, nada compila, y es a propósito: `parser.rkt` construye
`(not-exp $3)` y `not-exp` no existe todavía.

```
$ racket main.rkt
parser.rkt:225:62: not-exp: unbound identifier
```

Lo que sí corre suelto, porque no toca `ast.rkt`:

```
$ raco test tests/vals-tests.rkt tests/env-tests.rkt tests/lexer-tests.rkt
69 tests passed
```

Con los cinco `struct` escritos y ninguna cláusula, todo compila y
`raco test tests` da `10/268 test failures`. Con todas las cláusulas, `268
tests passed`.

## Cómo se corre

Desde esta carpeta:

```
$ raco test tests
$ racket main.rkt
```
