(module
  scm-utils
  (
   divides?
   foreach/enum
   prime-factors
   primes
   primes<
   primes<=
   undefined
   )

  (import scheme)
  (import chicken.base)
  (import chicken.module)

  (import srfi-41)
  (import srfi-42)
  (import streams-derived)
  (import streams-math)

  (reexport srfi-41)
  (reexport srfi-42)
  (reexport streams-derived)
  (reexport streams-math)

  (define (undefined)
    (error 'undefined))

  (define (++ n) (+ n 1))
  (define (-- n) (- n 1))

  (define (foreach/enum f l)
    (let loop ((idx 0)
               (l l))
      (unless (null? l)
        (let ((head (car l))
              (tail (cdr l)))
          (f idx head)
          (loop (++ idx) tail)))))

  (define (divides? m n)
    (or (= m n)
        (and (< m n)
             (zero? (modulo n m)))))

  (define (primes<  n) (stream-take-while (cut <  <> n) prime-numbers-stream))
  (define (primes<= n) (stream-take-while (cut <= <> n) prime-numbers-stream))
  (define (primes #!optional (n #f)) (if n (stream-take n prime-numbers-stream) prime-numbers-stream))

  (define (prime-factors n #!optional (inc-order? #f))
    (define (prime-factors-iter n primes ret)
      (cond
        ((= n 1)
         (if inc-order? (reverse ret) ret))
        ((divides? (stream-car primes) n)
         (prime-factors-iter (/ n (stream-car primes))
                             primes
                             (cons (stream-car primes) ret)))
        (else
          (prime-factors-iter n (stream-cdr primes) ret))))

    (define (take2s n)
      (let loop ((n n) (ret '()))
        (if (even? n)
            (loop (/ n 2) (cons 2 ret))
            `(,n . ,ret))))

    (and (> n 1)
         (let ((nd (take2s n)))
           (prime-factors-iter (car nd) (primes) (cdr nd)))))
  )
