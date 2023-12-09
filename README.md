"# PROLOG_20121037"
"ejemplo prolog"

# Tarea de áreas y volúmenes:

```lisp
;Áreas y volúmenes Tarea por Fernando Getsemaní Santoyo Corona

;áreas de figuras

;cuadrado
(defun a-cuadrado ()
  (princ "Dame el tamaño de un lado: ")
  (setq lado (read))
  (princ "El área del cuadrado es: ")
  (princ (* lado lado)))

;rectángulo
(defun a-rectangulo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "El área del rectángulo es: ")
  (princ (* longitud ancho)))

;triángulo
(defun a-triangulo ()
  (princ "Dame la base: ")
  (setq base (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del triángulo es: ")
  (princ (/ (* base altura) 2)))

;círculo
(defun a-circulo ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "El área del círculo es: ")
  (princ (* pi (* radio radio))))

;trapecio
(defun a-trapecio ()
  (princ "Dame la longitud de la base mayor: ")
  (setq base1 (read))
  (princ "Dame la longitud de la base menor: ")
  (setq base2 (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del trapecio es: ")
  (princ (/ (* (+ base1 base2) altura) 2)))

;rombo
(defun a-rombo ()
  (princ "Dame la longitud de la diagonal mayor: ")
  (setq diagonalMayor (read))
  (princ "Dame la longitud de la diagonal menor: ")
  (setq diagonalMenor (read))
  (princ "El área del rombo es: ")
  (princ (/ (* diagonalMayor diagonalMenor) 2)))

;pentágono regular
(defun a-pentagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del pentágono regular es: ")
  (princ (* (/ 5 4) (* lado lado (/ 1 (tan (/ pi 5)))))))

;hexágono regular
(defun a-hexagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del hexágono regular es: ")
  (princ (/ (* 3 (sqrt 3) (* lado lado)) 2)))

;tiángulo equilátro
(defun a-triangulo-equilatero ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del triángulo equilátero es: ")
  (princ (/ (* (* lado lado) (sqrt 3)) 4)))

;sector circular
(defun a-sector-circular ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "Dame el ángulo (en grados): ")
  (setq angulo (read))
  (princ "El área del sector circular es: ")
  (princ (* (/ angulo 360.0) pi (* radio radio))))


; Funciones para calcular el volumen

;cubo
(defun v-cubo ()
  (princ "Dame el lado del cubo: ")
  (setq lado (read))
  (princ "El volumen del cubo es: ")
  (princ (expt lado 3)))

;paralepipedo
(defun v-paralelepipedo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El volumen del paralelepípedo es: ")
  (princ (* longitud ancho altura)))

;prisma trinagular
(defun v-prisma-triangular ()
  (princ "Dame la base del prisma triangular: ")
  (setq base (read))
  (princ "Dame la altura del prisma triangular: ")
  (setq altura (read))
  (princ "Dame la longitud del prisma triangular: ")
  (setq longitud (read))
  (princ "El volumen del prisma triangular es: ")
  (princ (/ (* base altura longitud) 2)))

;cilindro
(defun v-cilindro ()
  (princ "Dame el radio del cilindro: ")
  (setq radio (read))
  (princ "Dame la altura del cilindro: ")
  (setq altura (read))
  (princ "El volumen del cilindro es: ")
  (princ (* pi (* radio radio) altura)))

;cono
(defun v-cono ()
  (princ "Dame el radio del cono: ")
  (setq radio (read))
  (princ "Dame la altura del cono: ")
  (setq altura (read))
  (princ "El volumen del cono es: ")
  (princ (/ (* pi (* radio radio) altura) 3)))

;pirámide cuadrangular
(defun v-piramide-cuadrangular ()
  (princ "Dame la base de la pirámide cuadrangular: ")
  (setq base (read))
  (princ "Dame la altura de la pirámide cuadrangular: ")
  (setq altura (read))
  (princ "El volumen de la pirámide cuadrangular es: ")
  (princ (/ (* base altura) 3)))

;esfera
(defun v-esfera ()
  (princ "Dame el radio de la esfera: ")
  (setq radio (read))
  (princ "El volumen de la esfera es: ")
  (princ (/ (* 4 pi (expt radio 3)) 3)))

;cilindro hueco
(defun v-cilindro-hueco ()
  (princ "Dame el radio exterior del cilindro: ")
  (setq R (read))
  (princ "Dame el radio interior del cilindro: ")
  (setq r (read))
  (princ "Dame la altura del cilindro hueco: ")
  (setq altura (read))
  (princ "El volumen del cilindro hueco es: ")
  (princ (* pi (- (expt R 2) (expt r 2)) altura)))

;cono truncado
(defun v-cono-truncado ()
  (princ "Dame el radio mayor del cono truncado: ")
  (setq R (read))
  (princ "Dame el radio menor del cono truncado: ")
  (setq r (read))
  (princ "Dame la altura del cono truncado: ")
  (setq altura (read))
  (princ "El volumen del cono truncado es: ")
  (princ (/ (* pi (+ (expt R 2) (expt r 2))))))


```

# Tarea de Áreas y volúmenes Input function added by 'read'

```lisp

;áreas de figuras

;cuadrado
(defun a-cuadrado ()
  (princ "Dame el tamaño de un lado: ")
  (setq lado (read))
  (princ "El área del cuadrado es: ")
  (princ (* lado lado)))

;rectángulo
(defun a-rectangulo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "El área del rectángulo es: ")
  (princ (* longitud ancho)))

;triángulo
(defun a-triangulo ()
  (princ "Dame la base: ")
  (setq base (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del triángulo es: ")
  (princ (/ (* base altura) 2)))

;círculo
(defun a-circulo ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "El área del círculo es: ")
  (princ (* pi (* radio radio)))
)

;trapecio
(defun a-trapecio ()
  (princ "Dame la longitud de la base mayor: ")
  (setq base1 (read))
  (princ "Dame la longitud de la base menor: ")
  (setq base2 (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del trapecio es: ")
  (princ (/ (* (+ base1 base2) altura) 2)))

;rombo
(defun a-rombo ()
  (princ "Dame la longitud de la diagonal mayor: ")
  (setq diagonalMayor (read))
  (princ "Dame la longitud de la diagonal menor: ")
  (setq diagonalMenor (read))
  (princ "El área del rombo es: ")
  (princ (/ (* diagonalMayor diagonalMenor) 2)))

;pentágono regular
(defun a-pentagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del pentágono regular es: ")
  (princ (* (/ 5 4) (* lado lado (/ 1 (tan (/ pi 5)))))))

;hexágono regular
(defun a-hexagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del hexágono regular es: ")
  (princ (/ (* 3 (sqrt 3) (* lado lado)) 2)))

;tiángulo equilátro
(defun a-triangulo-equilatero ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del triángulo equilátero es: ")
  (princ (/ (* (* lado lado) (sqrt 3)) 4)))

;sector circular
(defun a-sector-circular ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "Dame el ángulo (en grados): ")
  (setq angulo (read))
  (princ "El área del sector circular es: ")
  (princ (* (/ angulo 360.0) pi (* radio radio))))


; Funciones para calcular el volumen

;cubo
(defun v-cubo ()
  (princ "Dame el lado del cubo: ")
  (setq lado (read))
  (princ "El volumen del cubo es: ")
  (princ (expt lado 3)))

;paralepipedo
(defun v-paralelepipedo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El volumen del paralelepípedo es: ")
  (princ (* longitud ancho altura)))

;prisma trinagular
(defun v-prisma-triangular ()
  (princ "Dame la base del prisma triangular: ")
  (setq base (read))
  (princ "Dame la altura del prisma triangular: ")
  (setq altura (read))
  (princ "Dame la longitud del prisma triangular: ")
  (setq longitud (read))
  (princ "El volumen del prisma triangular es: ")
  (princ (/ (* base altura longitud) 2)))

;cilindro
(defun v-cilindro ()
  (princ "Dame el radio del cilindro: ")
  (setq radio (read))
  (princ "Dame la altura del cilindro: ")
  (setq altura (read))
  (princ "El volumen del cilindro es: ")
  (princ (* pi (* radio radio) altura)))

;cono
(defun v-cono ()
  (princ "Dame el radio del cono: ")
  (setq radio (read))
  (princ "Dame la altura del cono: ")
  (setq altura (read))
  (princ "El volumen del cono es: ")
  (princ (/ (* pi (* radio radio) altura) 3)))

;pirámide cuadrangular
(defun v-piramide-cuadrangular ()
  (princ "Dame la base de la pirámide cuadrangular: ")
  (setq base (read))
  (princ "Dame la altura de la pirámide cuadrangular: ")
  (setq altura (read))
  (princ "El volumen de la pirámide cuadrangular es: ")
  (princ (/ (* base altura) 3)))

;esfera
(defun v-esfera ()
  (princ "Dame el radio de la esfera: ")
  (setq radio (read))
  (princ "El volumen de la esfera es: ")
  (princ (/ (* 4 pi (expt radio 3)) 3)))

;cilindro hueco
(defun v-cilindro-hueco ()
  (princ "Dame el radio exterior del cilindro: ")
  (setq R (read))
  (princ "Dame el radio interior del cilindro: ")
  (setq r (read))
  (princ "Dame la altura del cilindro hueco: ")
  (setq altura (read))
  (princ "El volumen del cilindro hueco es: ")
  (princ (* pi (- (expt R 2) (expt r 2)) altura)))

;cono truncado
(defun v-cono-truncado ()
  (princ "Dame el radio mayor del cono truncado: ")
  (setq R (read))
  (princ "Dame el radio menor del cono truncado: ")
  (setq r (read))
  (princ "Dame la altura del cono truncado: ")
  (setq altura (read))
  (princ "El volumen del cono truncado es: ")
  (princ (/ (* pi (+ (expt R 2) (expt r 2))))))



```

# Tarea áreas y volúmenes usando Lambdas

```lisp

;ÁREAS:

(defvar cuadrado (lambda (a)
	(* a a)
))

(defvar rectangulo (lambda (b a)(
	* b a
)))

(defvar triangulo (lambda (b h)(
	/ (* b h) 2
)))

(defvar circulo (r)(
	* pi (* radio radio)
))

(defvar trapecio (a b h)(
	/ (* (+ a b) h) 2
))

(defvar rombo (lambda (diagonalMayor diagonalMenor) (/ (* diagonalMayor diagonalMenor) 2)))

(defvar pentagono-regular (lambda (lado) (* (/ 5 4) (* lado lado (/ 1 (tan (/ pi 5)))))))

(defvar hexagono-regular (lambda (lado) (/ (* 3 (sqrt 3) (* lado lado)) 2)))


(defvar triangulo-equilatero (lambda (lado) (/ (* (* lado lado) (sqrt 3)) 4)))

(defvar sector-circular (lambda (radio angulo) (* (/ angulo 360.0) pi (* radio radio)))))

;Volúmenes

(defvar cubo (lambda (lado) (expt lado 3)))

(defvar paralelepipedo (lambda (longitud ancho altura) (* longitud ancho altura)))

(defvar prisma-triangular (lambda (base altura longitud) (/ (* base altura longitud) 2)))

(defvar cilindro (lambda (radio altura) (* pi (* radio radio) altura)))

(defvar cono (lambda (radio altura) (/ (* pi (* radio radio) altura) 3)))

(defvar piramide-cuadrangular (lambda (base altura) (/ (* base altura) 3)))

(defvar esfera (lambda (radio) (/ (* 4 pi (expt radio 3)) 3)))

(defvar cilindro-hueco (lambda (R r altura) (* pi (- (expt R 2) (expt r 2)) altura)))

(defvar cono-truncado (lambda (R r) (/ (* pi (+ (expt R 2) (expt r 2))))))

```

# Sitema de árbol genealógico

```java

;sistema basado en la familia Simpson prolog

% Padres
padres(abraham, herbert).
padres(mona, herbert).
padres(herbert, homer).
padres(homer, bart).
padres(homer, lisa).
padres(homer, maggie).
padres(marge, bart).
padres(marge, lisa).
padres(marge, maggie).

% Reglas de descendencia
hijo(X, Y) :- padres(Y, X).
hija(X, Y) :- padres(Y, X).
madre(X, Y) :- padres(X, Y), hija(Y, _).
padre(X, Y) :- padres(X, Y), hijo(Y, _).
abuelo(X, Y) :- padres(X, Z), padres(Z, Y), (hijo(Y, _); hija(Y, _)).
abuela(X, Y) :- padres(X, Z), padres(Z, Y), (hijo(Y, _); hija(Y, _)).
hermano(X, Y) :- padres(Z, X), padres(Z, Y), hijo(X, _), hijo(Y, _), X \= Y.
hermana(X, Y) :- padres(Z, X), padres(Z, Y), hija(X, _), hija(Y, _), X \= Y.

pregunta(Respuesta) :-
    write('¿Quiere hacer una pregunta sobre los Simpson? (sí/no): '),
    read(Decision),
    (
        Decision = 'sí' ->
        hacer_pregunta(Respuesta)
        ;
        Respuesta = '¡Hasta luego!'
    ).

hacer_pregunta(Respuesta) :-
    write('¿Cuál es tu pregunta? '), nl,
    read(Pregunta),
    (
        responder(Pregunta, Respuesta),
        nl,
        write('La respuesta es: '),
        write(Respuesta), nl,
        pregunta(NuevaRespuesta)
        ;
        Respuesta = 'No entiendo la pregunta, por favor intenta otra vez.',
        nl,
        write(Respuesta), nl,
        hacer_pregunta(NuevaRespuesta)
    ).

responder(Pregunta, Respuesta) :-
    (
        call(Pregunta) ->
        Respuesta = 'sí'
        ;
        Respuesta = 'no'
    ).

:- pregunta(Respuesta).


```

# Eliza traducida al español

```java

eliza :-
    writeln('Hola, mi nombre es Eliza, tu chatbot. Por favor, ingresa tu consulta, usando solo minúsculas sin punto al final:'),
    readln(Input),
    eliza(Input),
    !.

eliza(Input) :- 
    Input == ['Adios'],
    writeln('Adiós. Espero haber podido ayudarte.'),
    !.

eliza(Input) :- 
    Input == ['Adios', '.'],
    writeln('Adiós. Espero haber podido ayudarte.'),
    !.

eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R),
    readln(Input1),
    eliza(Input1),
    !.

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, '¿Cómo', estás, tú, '?'], [4]).
template([buenos_días, mi, nombre, es, s(_), '.'], ['Buenos días', '¿Cómo', estás, tú, 0, '?'], [4]).
template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, '¿Cómo', estás, tú, '?'], [5]).
template([buenos_días, ',', mi, nombre, es, s(_), '.'], ['Buenos días', '¿Cómo', estás, tú, 0, '?'], [5]).
template([hola, _], ['Hola', '¿Cómo', estás, tú, '?'], []).
template([buenos_días, _], ['Buenos días', '¿Cómo', estás, tú, '?'], []).
template([yo, s(_), yo, soy, s(_),'.'], ['¿Por qué', eres, tú, 1, '?'], [4]).
template([yo, s(_), tú, '.'], ['¿Por qué', me, tú, '?'], [1]).
template([yo, soy, s(_),'.'], ['¿Por qué', eres, tú, 0, '?'], [2]).
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).
template([tu, eres, s(_), _], [flagDo], [2]).
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).
template([como, estas, tu, '?'], ['Yo estoy bien, gracias por preguntar.'], []).
template([yo, pienso, que, _], ['Bueno, esa es tu opinión.'], []).
template([porque, _], ['Esa no es una buena razón.'], []).
template([tengo, un, s(_), con, s(_), '.'], ['Tienes que lidiar con tu 0 y tu 1 de manera madura.'], [2, 4]).
template([yo, s(_),  _], ['Puedo recomendarte un libro sobre ello.'], []).
template([porfavor, s(_), _], ['No puedo ayudar, soy solo una máquina.'], []).
template([dime, _ , s(_), _], ['No, no puedo, soy mala en eso.'], []).
template(_, ['Por favor, explica un poco más.'], []).

elizaGusta(X, R) :- leGusta(X), R = ['Sí, me gusta', X].
elizaGusta(X, R) :- \+leGusta(X), R = ['No, no me gusta', X].
leGusta(manzanas).
leGusta(computadoras).
leGusta(carros).

elizaHace(X, R) :- hace(X), R = ['Sí, yo', X, 'y me encanta'].
elizaHace(X, R) :- \+hace(X), R = ['No, yo no', X, '. Es demasiado difícil para mí.'].
hace(estudiar).
hace(cocinar).
hace(trabajar).

elizaEs(X, R) :- es0(X), R = ['Sí, yo soy', X].
elizaEs(X, R) :- \+es0(X), R = ['No, yo no soy', X].
es0(tonta).
es0(rara).
es0(amable).
es0(feliz).

match([],[]).
match([], _):- true.
match([S|Stim],[I|Input]) :-
    atom(S),
    S == I,
    match(Stim, Input),
    !.
match([S|Stim],[_|Input]) :-
    \+atom(S),
    match(Stim, Input),
    !.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagLike,
    elizaGusta(Atom, R).
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagDo,
    elizaHace(Atom, R).
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagIs,
    elizaEs(Atom, R).
replace0([I|Index], Input, N, Resp, R):-
    length(Index, M), M =:= 0,
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1),
    append(R1, [], R),
    !.
replace0([I|Index], Input, N, Resp, R):-
    nth0(I, Input, Atom),
    length(Index, M), M > 0,
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R),
    !.


```

# Sistema para preguntar características de lenguaje de programación a eliza

```java

% Hechos sobre lenguajes de programación
lenguaje_programacion(c, estructurado).
lenguaje_programacion(cpp, orientado_objetos).
lenguaje_programacion(java, orientado_objetos).
lenguaje_programacion(python, interpretado).
lenguaje_programacion(python, de_alto_nivel).
lenguaje_programacion(python, scripting).
lenguaje_programacion(python, multiparadigma).
lenguaje_programacion(javascript, interpretado).
lenguaje_programacion(javascript, basado_prototipos).
lenguaje_programacion(javascript, web).
lenguaje_programacion(swift, orientado_objetos).
lenguaje_programacion(swift, funcional).
lenguaje_programacion(swift, compilado).
lenguaje_programacion(ruby, interpretado).
lenguaje_programacion(ruby, de_alto_nivel).
lenguaje_programacion(ruby, scripting).
lenguaje_programacion(go, compilado).
lenguaje_programacion(go, concurrente).
lenguaje_programacion(go, de_alto_nivel).
lenguaje_programacion(scala, funcional).
lenguaje_programacion(scala, orientado_objetos).
lenguaje_programacion(scala, compilado).
lenguaje_programacion(kotlin, orientado_objetos).
lenguaje_programacion(kotlin, interoperable_java).
lenguaje_programacion(kotlin, compilado).
lenguaje_programacion(pascal, estructurado).
lenguaje_programacion(pascal, compilado).
lenguaje_programacion(lisp, funcional).
lenguaje_programacion(lisp, interpretado).
lenguaje_programacion(haskell, funcional).
lenguaje_programacion(haskell, compilado).
lenguaje_programacion(clojure, funcional).
lenguaje_programacion(clojure, lisp).
lenguaje_programacion(clojure, compilado).
lenguaje_programacion(perl, interpretado).
lenguaje_programacion(perl, scripting).
lenguaje_programacion(perl, multiparadigma).
lenguaje_programacion(php, interpretado).
lenguaje_programacion(php, web).

% Preguntas y reglas sobre lenguajes de programación
template([es, un, lenguaje, de, programacion, orientado_objetos], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, interpretado], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, compilado], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, funcional], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, scripting], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, web], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, multiparadigma], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, basado_prototipos], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, concurrente], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, de_alto_nivel], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, interoperable_java], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, estructurado], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, lisp], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, clojure], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, perl], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, haskell], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, swift], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, kotlin], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, pascal], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, go], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, ruby], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, javascript], [flagFeature], [2]).
template([es, un, lenguaje, de, programacion, php], [flagFeature], [2]).

% Respuestas sobre lenguajes de programación
elizaFeature(X, R) :- lenguaje_programacion(X, Y), R = ['Sí,', X, 'es', Y].
elizaFeature(X, R) :- \+lenguaje_programacion(X, _), R = ['No,', X, 'no tiene esa característica.']


```


# Base de enfermedad para sistema maestro

```java

% Hechos sobre los síntomas del Síndrome de Moebius
sintoma(sindrome_moebius, parálisis_facial).
sintoma(sindrome_moebius, incapacidad_sonreir).
sintoma(sindrome_moebius, imposibilidad_movimientos_oculares).
sintoma(sindrome_moebius, dificultad_alimentarse).
sintoma(sindrome_moebius, estrabismo).
sintoma(sindrome_moebius, dificultad_habla).
sintoma(sindrome_moebius, hipotonía_muscular).
sintoma(sindrome_moebius, anomalías_piernas_pies).

% Hechos sobre las causas y factores de riesgo del Síndrome de Moebius
causa(sindrome_moebius, factores_geneticos).
causa(sindrome_moebius, anomalías_desarrollo_embrionario).
causa(sindrome_moebius, exposición_sustancias_toxicas).

% Tratamientos y terapias para el Síndrome de Moebius
tratamiento(sindrome_moebius, terapia_ocupacional).
tratamiento(sindrome_moebius, terapia_habla).
tratamiento(sindrome_moebius, cirugia_reconstructiva).
tratamiento(sindrome_moebius, fisioterapia).
tratamiento(sindrome_moebius, intervenciones_apoyo).

% Reglas para preguntas y respuestas sobre el Síndrome de Moebius
template([qué, es, el, síndrome, de, moebius, '?'], ['El Síndrome de Moebius es una enfermedad rara que afecta los nervios craneales, causando parálisis facial y limitaciones en los movimientos oculares y faciales.'], [1]).

template([cuáles, son, los, síntomas, del, síndrome, de, moebius, '?'], ['Los síntomas del Síndrome de Moebius incluyen parálisis facial, dificultades en los movimientos oculares, problemas de alimentación y habla, entre otros.'], [2]).

template([cuáles, son, las, causas, del, síndrome, de, moebius, '?'], ['Las causas pueden estar relacionadas con factores genéticos y anomalías durante el desarrollo embrionario, así como exposición a sustancias tóxicas en el embarazo.'], [3]).

template([cómo, se, trata, el, síndrome, de, moebius, '?'], ['El tratamiento incluye terapias de rehabilitación como terapia ocupacional, terapia del habla, cirugía reconstructiva y fisioterapia para mejorar la calidad de vida del paciente.'], [4]).

template([cuál, es, el, pronóstico, del, síndrome, de, moebius, '?'], ['El pronóstico varía según la severidad de los síntomas, pero con intervenciones tempranas y tratamiento adecuado, se puede mejorar la calidad de vida de quienes lo padecen.'], [5]).


```