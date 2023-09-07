"# PROLOG_20121037" 
"ejemplo prolog"

# Tarea de áread y volúmenes:
```

(defun a-cuadrado ()
  (princ "Dame el tamaño de un lado: ")
  (setq lado (read))
  (princ "El área del cuadrado es: ")
  (princ (* lado lado)))

(defun a-rectangulo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "El área del rectángulo es: ")
  (princ (* longitud ancho)))

(defun a-triangulo ()
  (princ "Dame la base: ")
  (setq base (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del triángulo es: ")
  (princ (/ (* base altura) 2)))

(defun a-circulo ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "El área del círculo es: ")
  (princ (* pi (* radio radio))))

(defun a-trapecio ()
  (princ "Dame la longitud de la base mayor: ")
  (setq base1 (read))
  (princ "Dame la longitud de la base menor: ")
  (setq base2 (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El área del trapecio es: ")
  (princ (/ (* (+ base1 base2) altura) 2)))

(defun a-rombo ()
  (princ "Dame la longitud de la diagonal mayor: ")
  (setq diagonalMayor (read))
  (princ "Dame la longitud de la diagonal menor: ")
  (setq diagonalMenor (read))
  (princ "El área del rombo es: ")
  (princ (/ (* diagonalMayor diagonalMenor) 2)))

(defun a-pentagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del pentágono regular es: ")
  (princ (* (/ 5 4) (* lado lado (/ 1 (tan (/ pi 5)))))))

(defun a-hexagono-regular ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del hexágono regular es: ")
  (princ (/ (* 3 (sqrt 3) (* lado lado)) 2)))

(defun a-triangulo-equilatero ()
  (princ "Dame la longitud del lado: ")
  (setq lado (read))
  (princ "El área del triángulo equilátero es: ")
  (princ (/ (* (* lado lado) (sqrt 3)) 4)))

(defun a-sector-circular ()
  (princ "Dame el radio: ")
  (setq radio (read))
  (princ "Dame el ángulo (en grados): ")
  (setq angulo (read))
  (princ "El área del sector circular es: ")
  (princ (* (/ angulo 360.0) pi (* radio radio))))

;; Funciones para calcular el volumen

(defun v-cubo ()
  (princ "Dame el lado del cubo: ")
  (setq lado (read))
  (princ "El volumen del cubo es: ")
  (princ (expt lado 3)))

(defun v-paralelepipedo ()
  (princ "Dame la longitud: ")
  (setq longitud (read))
  (princ "Dame el ancho: ")
  (setq ancho (read))
  (princ "Dame la altura: ")
  (setq altura (read))
  (princ "El volumen del paralelepípedo es: ")
  (princ (* longitud ancho altura)))

(defun v-prisma-triangular ()
  (princ "Dame la base del prisma triangular: ")
  (setq base (read))
  (princ "Dame la altura del prisma triangular: ")
  (setq altura (read))
  (princ "Dame la longitud del prisma triangular: ")
  (setq longitud (read))
  (princ "El volumen del prisma triangular es: ")
  (princ (/ (* base altura longitud) 2)))

(defun v-cilindro ()
  (princ "Dame el radio del cilindro: ")
  (setq radio (read))
  (princ "Dame la altura del cilindro: ")
  (setq altura (read))
  (princ "El volumen del cilindro es: ")
  (princ (* pi (* radio radio) altura)))

(defun v-cono ()
  (princ "Dame el radio del cono: ")
  (setq radio (read))
  (princ "Dame la altura del cono: ")
  (setq altura (read))
  (princ "El volumen del cono es: ")
  (princ (/ (* pi (* radio radio) altura) 3)))

(defun v-piramide-cuadrangular ()
  (princ "Dame la base de la pirámide cuadrangular: ")
  (setq base (read))
  (princ "Dame la altura de la pirámide cuadrangular: ")
  (setq altura (read))
  (princ "El volumen de la pirámide cuadrangular es: ")
  (princ (/ (* base altura) 3)))

(defun v-esfera ()
  (princ "Dame el radio de la esfera: ")
  (setq radio (read))
  (princ "El volumen de la esfera es: ")
  (princ (/ (* 4 pi (expt radio 3)) 3)))

(defun v-cilindro-hueco ()
  (princ "Dame el radio exterior del cilindro: ")
  (setq R (read))
  (princ "Dame el radio interior del cilindro: ")
  (setq r (read))
  (princ "Dame la altura del cilindro hueco: ")
  (setq altura (read))
  (princ "El volumen del cilindro hueco es: ")
  (princ (* pi (- (expt R 2) (expt r 2)) altura)))

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
