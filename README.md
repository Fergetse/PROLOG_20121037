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



# Eliza + Síndrome + Recetas

```java

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sección de Eliza
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eliza :-
    writeln('Hola, mi nombre es Eliza, tu chatbot. Por favor, ingresa tu consulta, usando solo minusculas sin punto al final:'),
    readln(Input),
    eliza(Input),
    !.

eliza(Input) :- 
    Input == ['Adios'],
    writeln('Adios. Espero haber podido ayudarte.'),
    !.

eliza(Input) :- 
    Input == ['Adios', '.'],
    writeln('Adios. Espero haber podido ayudarte.'),
    !.

eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R),
    readln(Input1),
    eliza(Input1),
    !.

template([hola, mi, nombre, es, s(_), '.'], ['Hola', 0, '¿Como', estas, tu, '?'], [4]).
template([buenos_dias, mi, nombre, es, s(_), '.'], ['Buenos dias', '¿Como', estas, tu, 0, '?'], [4]).
template([hola, ',', mi, nombre, es, s(_), '.'], ['Hola', 0, '¿Como', estas, tu, '?'], [5]).
template([buenos_dias, ',', mi, nombre, es, s(_), '.'], ['Buenos dias', '¿Como', estas, tu, 0, '?'], [5]).
template([hola, _], ['Hola', '¿Como', estas, tu, '?'], []).
template([buenos_dias, _], ['Buenos dias', '¿Como', estas, tu, '?'], []).
template([yo, s(_), yo, soy, s(_),'.'], ['¿Por que', eres, tu, 1, '?'], [4]).
template([yo, s(_), tu, '.'], ['¿Por que', me, tu, '?'], [1]).
template([yo, soy, s(_),'.'], ['¿Por que', eres, tu, 0, '?'], [2]).
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).
template([tu, eres, s(_), _], [flagDo], [2]).
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).
template([como, estas, tu, '?'], ['Yo estoy bien, gracias por preguntar.'], []).
template([yo, pienso, que, _], ['Bueno, esa es tu opinion.'], []).
template([porque, _], ['Esa no es una buena razon.'], []).
template([tengo, un, s(_), con, s(_), '.'], ['Tienes que lidiar con tu 0 y tu 1 de manera madura.'], [2, 4]).
template([yo, s(_),  _], ['Puedo recomendarte un libro sobre ello.'], []).
template([porfavor, s(_), _], ['No puedo ayudar, soy solo una maquina.'], []).
template([dime, _ , s(_), _], ['No, no puedo, soy mala en eso.'], []).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sección del Sindrome de Moebius
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hechos sobre los sintomas del Sindrome de Moebius
sintoma(sindrome_moebius, paralisis_facial).
sintoma(sindrome_moebius, incapacidad_sonreir).
sintoma(sindrome_moebius, imposibilidad_movimientos_oculares).
sintoma(sindrome_moebius, dificultad_alimentarse).
sintoma(sindrome_moebius, estrabismo).
sintoma(sindrome_moebius, dificultad_habla).
sintoma(sindrome_moebius, hipotonia_muscular).
sintoma(sindrome_moebius, anomalias_piernas_pies).
sintoma(sindrome_moebius, micrognatia).
sintoma(sindrome_moebius, retraso_desarrollo_motor).
sintoma(sindrome_moebius, dificultad_respiratoria).
sintoma(sindrome_moebius, babeo_excesivo).
sintoma(sindrome_moebius, problemas_sensibilidad_facial).
sintoma(sindrome_moebius, problemas_visuales).
sintoma(sindrome_moebius, dificultad_succionar).
sintoma(sindrome_moebius, irritabilidad).
sintoma(sindrome_moebius, dificultades_sociales_relacionales).
sintoma(sindrome_moebius, dificultad_tragar_saliva).
sintoma(sindrome_moebius, desarrollo_dental_anormal).
sintoma(sindrome_moebius, dificultad_mover_ojos_en_conjunto).


% Hechos sobre las causas y factores de riesgo del Sindrome de Moebius
causa(sindrome_moebius, factores_geneticos).
causa(sindrome_moebius, anomalias_desarrollo_embrionario).
causa(sindrome_moebius, exposicion_sustancias_toxicas).
causa(sindrome_moebius, ingesta_medicamentos_embarazo).
causa(sindrome_moebius, lesiones_danio_utero).
causa(sindrome_moebius, anomalias_formacion_tronco_encefalico).
causa(sindrome_moebius, influencia_virus_infecciones).
causa(sindrome_moebius, factores_ambientales_quimicos).
causa(sindrome_moebius, desordenes_vasculares_embarazo).
causa(sindrome_moebius, predisposiciones_geneticas_ambientales).
causa(sindrome_moebius, falta_suministro_sanguineo_feto).
causa(sindrome_moebius, cambios_expresion_genica).
causa(sindrome_moebius, influencias_hormonales_embarazo).
causa(sindrome_moebius, alteraciones_formacion_vasos_sanguineos).
causa(sindrome_moebius, factores_epigeneticos).
causa(sindrome_moebius, anomalias_cromosomicas).
causa(sindrome_moebius, consumo_alcohol_embarazo).
causa(sindrome_moebius, exposicion_agentes_teratogenicos).
causa(sindrome_moebius, influencia_nutricion_materna).
causa(sindrome_moebius, interaccion_factores_geneticos_ambientales).


% Tratamientos y terapias para el Sindrome de Moebius
tratamiento(sindrome_moebius, terapia_ocupacional).
tratamiento(sindrome_moebius, terapia_habla).
tratamiento(sindrome_moebius, cirugia_reconstructiva).
tratamiento(sindrome_moebius, fisioterapia).
tratamiento(sindrome_moebius, intervenciones_apoyo).
tratamiento(sindrome_moebius, terapia_visual).
tratamiento(sindrome_moebius, ortodoncia).
tratamiento(sindrome_moebius, cuidado_nutricional).
tratamiento(sindrome_moebius, apoyo_psicologico).
tratamiento(sindrome_moebius, educacion_especializada).
tratamiento(sindrome_moebius, terapia_respiratoria).
tratamiento(sindrome_moebius, manejo_dolor).
tratamiento(sindrome_moebius, estimulacion_temprana).
tratamiento(sindrome_moebius, adaptaciones_ergonomicas).
tratamiento(sindrome_moebius, tecnologias_asistencia).
tratamiento(sindrome_moebius, soporte_social_familiar).
tratamiento(sindrome_moebius, seguimiento_medico).
tratamiento(sindrome_moebius, cuidado_dental_especializado).
tratamiento(sindrome_moebius, readaptacion_social).
tratamiento(sindrome_moebius, programas_inclusivos).


% Reglas para preguntas y respuestas sobre el Sindrome de Moebius

template([que, es, el, sindrome, de, moebius, '?'], ['El Sindrome de Moebius es una enfermedad rara que afecta los nervios craneales, causando paralisis facial y limitaciones en los movimientos oculares y faciales.'], []).

template([cuales, son, los, sintomas, del, sindrome, de, moebius, '?'], ['Los sintomas del Sindrome de Moebius incluyen: ', SINTOMAS], []) :- findall(X, sintoma(sindrome_moebius, X), SINTOMAS).

template([cuales, son, las, causas, del, sindrome, de, moebius, '?'], ['Algunas de las causas son: ', CAUSAS], []) :- findall(X, causa(sindrome_moebius, X), CAUSAS) .

template([como, se, trata, el, sindrome, de, moebius, '?'], ['El tratamiento puede incluir diversas alternativas como: ', TRATA], []) :- findall(X, tratamiento(sindrome_moebius, X), TRATA).



template([cual, es, el, pronostico, del, sindrome, de, moebius, '?'], ['El pronostico varia segun la severidad de los sintomas, pero con intervenciones tempranas y tratamiento adecuado, se puede mejorar la calidad de vida de quienes lo padecen.'], []).





template([si, tengo, s(_), podria, ser, sindrome, de, moebius, '?'], [flag_moebius],[2]).

es_moebius(X, R):- sintoma(_,X), R = [ X, es, un, sintoma, del, sindrome, de, moebius,., es, probable, que, lo, tenga, consulte, a, su, medico].
es_moebius(X, R):- \+sintoma(_,X), R = [X, no, es, un, sintoma, del, sindrome, de, moebius].

%trigger para síntoma
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flag_moebius,
    es_moebius(Atom, R).





template([si, tengo, sindrome, de, moebius, y, aplico, T, podre, mejorar, '?'], [flag_trat], [7]).

look_trate(X, R):- tratamiento(_,X), R = [ X, es, un, tratamiento, del, sindrome, de, moebius, por, ello, podras, mejorar, si, le, das, seguimiento].
look_trate(X, R):- \+tratamiento(_,X), R = [X, no, es, un, tratamiento, del, sindrome, de, moebius, no, mejoraras].

replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flag_trat,
    look_trate(Atom, R).




template([tengo, s(_), ',', (_), y, (_), podria, ser, sindrome, de, moebius, '?'], [flagVariosintomas], [1, 3, 5]).

multisintomas(X, Y, Z, R):- multi_sintoma(X, Y, Z), R = [ X, Y, y, Z, son, sintomas, del, sindrome, de, moebius, ., "Por favor consulte a su medico cuanto antes."].

multisintomas(X, Y, Z, R):- \+multi_sintoma(X, Y, Z), R = [ X, Y, y, Z, algunos, son, sintomas, del, sindrome, de, moebius, ., "Le recomiendo consultar a un medico."].

multisintomas(X, Y, Z, R):- \+multi_sintoma(X, Y, Z), R = [ X, Y, y, Z, no, son, sintomas, del, sindrome, de, moebius,.].


multi_sintoma(X, Y, Z) :- sintoma(sindrome_moebius, X), sintoma(sindrome_moebius, Y), sintoma(sindrome_moebius, Z).


% trigger para 3 síntomas del síndrome
replace0([I,J,K|_], Input, _, Resp, R):-
	nth0(I, Input, Atom),
	nth0(0, Resp, X),
	X == flagVariosintomas,
	nth0(J, Input, Atom2),
	nth0(0, Resp, Y),
	Y == flagVariosintomas,
	nth0(K, Input, Atom3),
	nth0(0, Resp, Z),
	Z == flagVariosintomas,
	multisintomas(Atom, Atom2, Atom3, R).

replace0([I|Index], Input, N, Resp, R):-
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R).

replace0([], _, _, Resp, Resp).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sección de Recetas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Definicion de ingredientes en recetas
ingrediente(sal).
ingrediente(azucar).
ingrediente(harina).
ingrediente(huevo).
ingrediente(mantequilla).
ingrediente(leche).
ingrediente(chocolate).
ingrediente(vainilla).
ingrediente(fresas).
ingrediente(canela).
ingrediente(pollo).
ingrediente(arroz).
ingrediente(aceite).
ingrediente(pasta).
ingrediente(queso).
ingrediente(cebolla).
ingrediente(tomate).
ingrediente(pimiento).
ingrediente(pimienta).
ingrediente(ajo).
ingrediente(salmon).
ingrediente(manzana).

% Definicion de tipos de cocina
tipo_cocina(italiana).
tipo_cocina(mexicana).
tipo_cocina(china).
tipo_cocina(francesa).
tipo_cocina(india).
tipo_cocina(japonesa).
tipo_cocina(vegana).
tipo_cocina(gluten_free).
tipo_cocina(postres).
tipo_cocina(desayunos).
tipo_cocina(aperitivos).
tipo_cocina(ensaladas).
tipo_cocina(platos_principales).
tipo_cocina(batidos).
tipo_cocina(sopas).

% Definicion de platos
plato(pizza).
plato(tacos).
plato(sushi).
plato(crepas).
plato(pollo_asado).
plato(lasagna).
plato(tortilla_espanola).
plato(pad_thai).
plato(tarta_manzana).
plato(pastel_chocolate).
plato(arroz_tres_delicias).
plato(ensalada_ceasar).
plato(omelette).
plato(brownies).
plato(muffins).
plato(pancakes).
plato(quesadillas).
plato(gazpacho).
plato(ratatouille).

% Relación entre platos y tipos de cocina
pertenece_a_tipo_de_cocina(pizza, italiana).
pertenece_a_tipo_de_cocina(tacos, mexicana).
pertenece_a_tipo_de_cocina(sushi, japonesa).
pertenece_a_tipo_de_cocina(crepas, francesa).
pertenece_a_tipo_de_cocina(pollo_asado, platos_principales).
pertenece_a_tipo_de_cocina(lasagna, italiana).
pertenece_a_tipo_de_cocina(tortilla_espanola, desayunos).
pertenece_a_tipo_de_cocina(pad_thai, tailandesa). 
pertenece_a_tipo_de_cocina(tarta_manzana, postres).
pertenece_a_tipo_de_cocina(pastel_chocolate, postres).
pertenece_a_tipo_de_cocina(arroz_tres_delicias, china).
pertenece_a_tipo_de_cocina(ensalada_ceasar, ensaladas).
pertenece_a_tipo_de_cocina(omelette, desayunos).
pertenece_a_tipo_de_cocina(brownies, postres).
pertenece_a_tipo_de_cocina(muffins, desayunos).
pertenece_a_tipo_de_cocina(pancakes, desayunos).
pertenece_a_tipo_de_cocina(quesadillas, mexicana).
pertenece_a_tipo_de_cocina(gazpacho, sopas).
pertenece_a_tipo_de_cocina(ratatouille, platos_principales).

% Definicion de recetas
receta(pizza, [harina, levadura, tomate, queso, pepperoni, oregano]).
receta(tacos, [tortilla_maiz, carne_asada, cebolla, cilantro, salsa]).
receta(sushi, [arroz, alga_nori, pescado, aguacate, pepino]).
receta(crepas, [harina, huevo, leche, mantequilla, fresas, chocolate]).
receta(pollo_asado, [pollo, limon, ajo, aceite_oliva, pimienta]).
receta(lasagna, [pasta_lasagna, carne_molida, tomate, queso_rallado]).
receta(tortilla_espanola, [patatas, huevo, cebolla, aceite]).
receta(pad_thai, [fideos_arroz, pollo, gambas, cacahuetes, brotes]).
receta(tarta_manzana, [manzanas, azucar, harina, mantequilla, canela]).
receta(pastel_chocolate, [harina, azucar, chocolate, mantequilla, huevo]).
receta(arroz_tres_delicias, [arroz, huevo, jamon, gambas, guisantes]).
receta(ensalada_ceasar, [lechuga, pollo, queso_parmesano, pan_tostado]).
receta(omelette, [huevo, cebolla, tomate, queso, espinacas]).
receta(brownies, [chocolate, azucar, harina, mantequilla, nueces]).
receta(muffins, [harina, azucar, huevo, leche, aceite, frutas]).
receta(pancakes, [harina, huevo, leche, azucar, vainilla]).
receta(quesadillas, [tortillas, queso, pollo, aguacate, crema]).
receta(gazpacho, [tomate, pepino, pimiento, ajo, aceite_oliva]).
receta(ratatouille, [berenjena, calabacin, tomate, cebolla, ajo]).




%Ingredientes de receta y cómo hacer cada receta
template([cuales, son, los, ingredientes, de, receta, R, '?'], [0, 'incluye', Ingredientes, '.'], [6]):- receta(R, Ingredientes).

template([quien, creo, plato, X, '?'], ['No tengo esa informacion sobre el creador de', 0, '.'], [3]).

template([como, hacer, X, '?'], ['Para hacer', 0, 'necesitas los siguientes ingredientes:', Ingredientes, '.'], [2]):- receta(X, Ingredientes).


% Pregunta por recetas que contienen tres ingredientes específicos
template([que, receta, puedo, hacer, con, A,',' ,B, y, C, '?'], ['Una buena receta que puedes hacer con', 0, 1, 'y', 2, 'es', Recetas, '.'], [5, 6, 8]) :- pregunta_recetas_con_tres_ingredientes(A, B, C, Recetas).


pregunta_recetas_con_tres_ingredientes(Ingrediente1, Ingrediente2, Ingrediente3, Recetas) :-
    receta(Receta, Ingredientes),
    member(Ingrediente1, Ingredientes),
    member(Ingrediente2, Ingredientes),
    member(Ingrediente3, Ingredientes),
    Ingredientes = [Ingrediente1, Ingrediente2, Ingrediente3 | _],
    Recetas = [Receta].


%Preguntar a qué categoría pertenece un plato
 template([a, que, categoria, pertenece, la, receta, R, '?'], [la, receta, 0, pertenece, a, la, categoria, CAT], [6]) :- pertenece_a_tipo_de_cocina(R, CAT).

 %preguntas generales
 template([], [], []).


%load("C:/Users/modim/Desktop/eliza2.pl").



template(_, ['Lo siento, no tengo informacion sobre eso.'], []).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reglas y procedimientos generales
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match([],[]).
match([], _) :- true.

match([S|Stim], [I|Input]) :-
    atom(S),
    S == I,
    match(Stim, Input),
    !.

match([S|Stim], [_|Input]) :-
    \+atom(S),
    match(Stim, Input),
    !.

replace0([], _, _, Resp, R) :-
    append(Resp, [], R),
    !.

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagLike,
    elizaGusta(Atom, R).

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagDo,
    elizaHace(Atom, R).

replace0([I|_], Input, _, Resp, R) :-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagIs,
    elizaEs(Atom, R).

replace0([I|Index], Input, N, Resp, R) :-
    length(Index, M), M =:= 0,
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1),
    append(R1, [], R),
    !.

replace0([I|Index], Input, N, Resp, R) :-
    nth0(I, Input, Atom),
    length(Index, M), M > 0,
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R),
    !.

elizaGusta(X, R) :-
    leGusta(X),
    R = ['Si, me gusta', X].

elizaGusta(X, R) :-
    \+leGusta(X),
    R = ['No, no me gusta', X].

leGusta(manzanas).
leGusta(computadoras).
leGusta(carros).

elizaHace(X, R) :-
    hace(X),
    R = ['Si, yo', X, 'y me encanta'].

elizaHace(X, R) :-
    \+hace(X),
    R = ['No, yo no', X, '. Es demasiado dificil para mi.'].

hace(estudiar).
hace(cocinar).
hace(trabajar).

elizaEs(X, R) :-
    es0(X),
    R = ['Si, yo soy', X].

elizaEs(X, R) :-
    \+es0(X),
    R = ['No, yo no soy', X].

es0(tonta).
es0(rara).
es0(amable).
es0(feliz).


```


# Preguntas realizables y consultas a sistema

## Recetas de cocina

1. cuales son los ingredientes de receta *tacos*?
2. quien creo plato *tacos*?
3. como hacer *tacos*?
4. que receta puedo hacer con *huevo*, *cebolla* y *tomate*?
5. a que categoria pertenece la receta *tacos*?


## Síndrome de Moebius

1. si tengo **tos** podria ser sindrome de moebius?
2. si tengo **tos**, **paralisis_facial** y **estrabismo** podria ser sindrome de moebius?
3. que es el sindrome de moebius?
4. cuales son los sintomas del sindrome de moebius?
5. cuales son las causas del sindrome de moebius?
6. como se trata el sindrome de moebius?
7. cual es el pronostico del sindrome de moebius?
8. si tengo sindrome de moebius y aplico **ortodoncia** podre mejorar?