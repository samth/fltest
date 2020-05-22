#lang racket/base

(require math/base
         math/bigfloat
         math/flonum
         racket/list
         racket/system
         racket/format
         racket/match
         racket/port)

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


(define js-rewrites
  (hash "expt" "pow"))

(define ns (make-base-namespace))
(parameterize ([current-namespace ns])
  (namespace-require 'racket/flonum)
  (namespace-require 'math/flonum)
  (namespace-require 'math/bigfloat))


(define (eval/values e)
  (call-with-values (lambda () (eval e)) (lambda args (if (= 1 (length args)) (car args) args))))

(define (print-result r)
  (if (list? r)
      (print-result (apply fl2->real* r))
      (if (bigfloat? r)
          (print-result (bigfloat->real* r))
          (~r r #:precision 30 #:notation 'exponential))))

(define (calculate-error er e*r*)
  (define e*r (if (bigfloat? e*r*) (bigfloat->real* e*r*) e*r*))
  (if (list? er)
      (values (fl2ulp-error (car er) (cadr er) e*r)
              (relative-error (fl2->real* (car er) (cadr er)) e*r)
              (absolute-error (fl2->real* (car er) (cadr er)) e*r))
      (values (flulp-error er e*r)
              (relative-error er e*r)
              (absolute-error er e*r))))

(define (fl2-op? op) (regexp-match "^fl2" (symbol->string op)))
(define (fl/error-op? op) (regexp-match "/error$" (symbol->string op)))

(define (->fl-op op)
  (define v1 (regexp-replace "^fl2" (symbol->string op) "fl"))
  (define v2 (regexp-replace "/error$" v1 ""))
  (string->symbol v2))

(define (->bf-op op)
  (string->symbol (regexp-replace "^fl" (symbol->string op) "bf")))

(define (convert-arg-pairs f args)
  (let loop ([args args])
    (if (null? args)
        null
        (cons (f (fl2->real* (car args) (cadr args)))
              (loop (cddr args))))))

(define (->bf-expr op args)
  (cons (->bf-op (->fl-op op))
        (if (fl2-op? op)
            (convert-arg-pairs bf args)
            (map bf args))))

(define (->fl-expr op args)
  (cons (->fl-op op)
        (if (fl2-op? op)
            (convert-arg-pairs real->double-flonum args)
            args)))

(define (show-error u rel abs prec #:extra [extra ""])
  (printf "error~a is ~s ulps (relative ~a, absolute ~a) with precision ~s\n"
          extra u (~r rel #:precision 20 #:notation 'exponential) (~r abs #:precision 20 #:notation 'exponential) prec))


(define (show v r max-len)
  (printf "~a => ~a\n" (~a v #:min-width max-len) (print-result r)))


(define (->js-expr expr)
  (match-define (list op args ...) expr)
  (define js-op-maybe (substring (symbol->string (->fl-op op)) 2))
  (define js-op (hash-ref js-rewrites js-op-maybe (lambda () js-op-maybe)))
  (format "Math.~a(~a)"
          js-op
          (apply ~a (add-between args ","))))

(define (run-js expr)
  (define s (open-output-string))
  ;(eprintf "nodejs -e \"console.log(~a)\"\n" expr)
  (define proc (parameterize ([current-output-port s])
                 (system (format "nodejs -e \"console.log((~a).toPrecision(30))\"" expr))))
  (define result (get-output-string s))
  ;(eprintf "result : ~s\n" result)
  (read (open-input-string result)))


(define (run test correct  js-expr prec #:ok? [ok? (lambda (u r a) #f #;(<= u 0.5))] [extra #f] [extra-message ""])
  (parameterize ([current-namespace ns])
    (define test-r (eval/values test))
    (define correct-r (eval/values correct))
    (define extra-r (and extra (eval/values extra)))
    (define max-len (apply max (map (compose string-length ~s) (list test-r correct extra js-expr))))
    (define-values (u rel abs) (calculate-error test-r correct-r))
    (define exact? (eqv? 0 u))
    (define js (and js-expr (run-js js-expr)))
    (define-values (js-u js-rel js-abs) (if js (calculate-error test-r js) (values +inf.0 #f #f)))
    (unless (or (ok? u rel abs) (< js-u u))
      (newline)
      (show test test-r max-len)
      (show correct correct-r max-len)
      (when js-expr ;(< js-u u)
        (show js-expr js max-len))
      (when extra
        (show extra extra-r max-len))
      (show-error u rel abs prec)
      (when js-expr
        (show-error js-u js-rel js-abs prec #:extra " when using js"))
      (when extra
        (define-values (u rel abs) (calculate-error extra-r correct-r))
        (show-error u rel abs prec #:extra extra-message)))))
  


(define (check e #:precision [prec 128])
  (parameterize ([bf-precision prec])
    (match e
      [(list op args ...)
       (define e* (->bf-expr op args))
       (define fl-expr (->fl-expr op args))
       (define js-expr (and (not (fl2-op? op)) (->js-expr fl-expr)))
       (if (or (fl/error-op? op) (fl2-op? op))
           (run e e* js-expr prec fl-expr " when just using floats")
           (run e e* js-expr prec))])))



(module+ checks
  (check '(flexp 1.0))
  (check '(flexp 1.0) #:precision 53)
  (check '(flexp 1.0) #:precision 500)

  (check '(flexpt 1.4916681462400412e-154 -1.0))

  (check '(flexpt 1.4916681462400412e-154 -1.0) #:precision 53)
  (check '(flexpt 1.4916681462400412e-154 -1.0) #:precision 5000)


  (check '(flsin 1.0508668734276366e+308))
  (check '(flsin 1.0508668734276366e+308) #:precision 53)
  (check '(flsin 1.0508668734276366e+308) #:precision 5000)

  (check '(flexpt -1.4916681462400412e-154 -1.0))

  (check '(flexpt 1.4916681462400412e-154 -1.0))
  (check '(flcos -1.0334262343793682e+207))

  (check '(fl2+ -6.999886226206346e+45 5.792628225761353e+29 6.9274150349997e+45 5.2105937810748895e+29) #:precision 500)
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
              (fl2+
                  5.754365118051034e+54
                  2.8307141643929314e+38
                  -5.753780528274009e+54
                  1.6423048070836051e+38)
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
  )
(define (explain s)
  (match-define (list r _ ...) (port->list read (open-input-string s)))
  (check r)
  (apply run-js r))

(module+ main
  (require racket/cmdline syntax/location)
  (command-line #:once-each
                [("--all") "run all" (dynamic-require (quote-module-path ".." checks) #f)]
                #:args ([arg #f])
                (when arg (explain arg))))
