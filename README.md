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

