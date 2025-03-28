;;; MBROT -- Generation of Mandelbrot set fractal.

(define (count r i step x y)

  (let ((max-count 64)
        (radius^2  16.0))

    (let ((cr (+ r (* (exact->inexact x) step)))
          (ci (+ i (* (exact->inexact y) step))))

      (let loop ((zr cr)
                 (zi ci)
                 (c 0))
        (if (= c max-count)
          c
          (let ((zr^2 (* zr zr))
                (zi^2 (* zi zi)))
            (if (> (+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (+ (- zr^2 zi^2) cr))
                    (new-zi (+ (* 2.0 (* zr zi)) ci)))
                (loop new-zr new-zi (+ c 1))))))))))

(define (mbrot matrix r i step n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
      (let loop2 ((x (- n 1)))
        (if (>= x 0)
          (begin
            (vector-set! (vector-ref matrix x) y (count r i step x y))
            (loop2 (- x 1)))
          (loop1 (- y 1)))))))

(define (test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (if (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(display (test 75))
(newline)
;5
