#lang racket

(require math/bigfloat racket/flonum
         math/flonum math/base
         math/private/flonum/flonum-functions
         math/private/utils/flonum-tests
         math/private/flonum/flonum-bits
         math/private/flonum/flonum-constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; not provided so copied

;; Like `fl2->real', but returns signed flonum zeros
(define (fl2->real* x2 x1)
  (define x.0 (fl+ x2 x1))
  (cond [(zero? x.0)  x2]
        [else  (fl2->real x2 x1)]))

(define (bigfloat->real* x)
  (define x.0 (bigfloat->flonum x))
  (cond [(fl= x.0 0.0)  x.0]
        [(flrational? x.0)  (bigfloat->real x)]
        [else  x.0]))

(define (fl2-error x2 x1 x)
  (cond [(not (fl2? x2 x1))  (list 'not-fl2? x2 x1)]
        [(different-zero? x2 x)  (list 'different-zero? x2 x)]
        [else  (fl2ulp-error x2 x1 x)]))

(define (different-zero? x y)
  (or (and (eqv? x -0.0) (eqv? y 0.0))
      (and (eqv? x 0.0) (eqv? y -0.0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-namespace-anchor ns)

(define table (hash 'flabs   'bfabs 
                    'flsqrt  'bfsqrt
                    'fllog   'bflog 
                    'flexp   'bfexp 
                    'flsin   'bfsin 
                    'flcos   'bfcos 
                    'fltan   'bftan 
                    'flasin  'bfasin
                    'flacos  'bfacos
                    'flatan  'bfatan
                    'fllog2  'bflog2
                    'flexpt  'bfexpt
                    'flexp/error 'bfexp
                    'fl2+    'bf+
                    'fl2- 'bf-
                    'fl2expm1 'bfexpm1
                    'fl2exp 'bfexp))

(current-namespace (namespace-anchor->namespace ns))

(define (eval/values e)
  (call-with-values (lambda () (eval e)) (lambda args (if (= 1 (length args)) (car args) args))))

(define (print-result r)
  (if (list? r)
      (print-result (apply fl2->real* r))
      (~r r #:precision 30 #:notation 'exponential)))

(define (calculate-error er e*r)
  (if (list? er)
      (values (fl2ulp-error (car er) (cadr er) e*r)
              (relative-error (fl2->real* (car er) (cadr er)) e*r))
      (values (flulp-error er e*r)
              (relative-error er e*r))))

(define (fl2-op? op) (memq op '(fl2+ fl2- fl2exp fl2expm1)))

(define (->bf op args)
  (if (fl2-op? op)
      (let loop ([args args])
        (if (null? args)
            null
            (cons (bf (fl2->real* (car args) (cadr args)))
                  (loop (cddr args)))))
      (map bf args)))

(define (check e #:precision [prec 128])
  (parameterize ([bf-precision prec])
    (match e
      [(list op args ...)
       (define e (cons op (map (lambda (e) (if (flonum? e) e (exact->inexact e))) args)))
       (define e* (cons (hash-ref table op) (->bf op args)))
       (define er (eval/values e))
       (define e*r (eval e*))
       (define e*s (~s e*))
       (newline)
       (printf "~a => ~a\n"
               (~s e #:min-width (string-length e*s))
               (print-result er))
       (printf "~a => ~a\n"
               e*s
               (~r (bigfloat->real* e*r) #:precision 30 #:notation 'exponential))
       (define-values (u rel) (calculate-error er (bigfloat->real* e*r)))
       (printf "error is ~s ulps (relative error ~a) with precision ~s\n"
                 u (~r rel #:precision 20 #:notation 'exponential) prec)
       
       (when (fl2-op? op)
         (define float-args 
           (let loop ([args args])
             (if (null? args)
                 null
                 (cons (real->double-flonum (fl2->real* (car args) (cadr args)))
                       (loop (cddr args))))))
         (define fl-op (string->symbol (regexp-replace "fl2" (symbol->string op) "fl")))
         (define fl-r (eval (cons fl-op float-args)))
         (printf "~a => ~a\n"
               (~s (cons fl-op float-args) #:min-width (string-length e*s))
               (print-result er))
         (define-values (u rel) (calculate-error fl-r (bigfloat->real* e*r)))
         (printf "error when just using floats is ~s ulps (relative error ~a) with precision ~s\n"
                 u (~r rel #:precision 20 #:notation 'exponential) prec))
         ])))




(check '(flexp 1.0))
(check '(flexp 1.0) #:precision 53)
(check '(flexp 1.0) #:precision 500)

(check '(flexpt 1.4916681462400412e-154 -1.0))

(check '(flexpt 1.4916681462400412e-154 -1.0) #:precision 53)
(check '(flexpt 1.4916681462400412e-154 -1.0) #:precision 5000)


(check '(flsin 1.0508668734276366e+308))
(check '(flsin 1.0508668734276366e+308) #:precision 53)
(check '(flsin 1.0508668734276366e+308) #:precision 5000)

;(flulp-error -0.7849352660212705 (flsin 1.0508668734276366e+308))

(check '(flexpt -1.4916681462400412e-154 -1.0))

(check '(flexpt 1.4916681462400412e-154 -1.0))
(check '(flcos -1.0334262343793682e+207))

(check '(fl2+ -6.999886226206346e+45 5.792628225761353e+29 6.9274150349997e+45 5.2105937810748895e+29)
       #:precision 500)
(check '(flexp/error 248.1221467462665))
(check '(fl2exp 247.7018220484832 -1.3307184342470623e-14))

(check '(fl2+ 4.185789606386705e+21 101632.05131793808 -4.207239989193863e+21 237655.07785361566))
(check '(flexp/error 247.4773606710883))
(check '(flexp/error -309.7938023915128))
(check '(flexp/error -249.59360697310464))
(check '(fl2exp 310.8807593548767 -8.308849114195227e-15))
(check '(fl2exp 251.859482911721 -1.155927414698983e-15))
(check '(fl2expm1 250.7410332680273 7.911227779561349e-15))
(check '(fl2expm1 253.64381400953715 1.2889509632926353e-14))

(for-each check
          '((fl2exp -253.05572964092207 -1.0193188793199029e-14)
            (fl2exp 247.08132868092463 -1.5500899213440343e-15)
            (fl2exp -311.0076112768378 -2.7337259662912242e-14)
            (fl2exp 313.1384665707421 2.1663247373169328e-14)
            (fl2expm1 251.98158723821487 1.2401437093586863e-14)
            (fl2expm1 250.8615080956815 -1.0435131192152885e-14)
            (fl2expm1 246.7336289130019 -1.224197841001902e-14)
            (fl2-
             -6.307625868783824e-189
             -7.650829856531877e-206
             -6.221347182541859e-189
             3.486864837367133e-205)
            (flexp/error -254.01518882090863)
            (flexp/error -251.95368322256186)
            (flexp/error -247.49730066045996)
            (flexp/error -306.356486488304)
            (flexp/error -247.29245235328085)
            (flexp/error -251.78502645604777)
            (flexp/error 249.18165493994528)
            (fl2exp 249.9020736185187 6.819710735232684e-15)
            (fl2exp 250.21789972727245 -8.899092268237878e-15)
            (fl2exp -311.9319594801789 -2.483683117656448e-14)
            (fl2expm1 253.6199382443308 1.0330029387260226e-14)
            (fl2expm1 251.75275370712401 -7.792688668380012e-15)))

