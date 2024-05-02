(define nil '())
(define defmacro (macro (name params &rest body) `(define ,name (macro  ,params (begin ,@body)))))
(define defun    (macro (name params &rest body) `(define ,name (lambda ,params (begin ,@body)))))

(define not (lambda (x) (if x #f #t)))
(define and (lambda (&rest y)
    (if (null? (cdr y))
        (car y)
        (if (car y)
            (apply and (cdr y))
            #f
    ))))
;(or #f #t #f) -> True
(define or (lambda (&rest y)
    (if (car y)
        #t
        (if (null? (cdr y))
            (car y)
            (apply or (cdr y))
    ))))
;(frac 12.34) -> 0.33999999999999986
(define frac (lambda (n) (- n (int n))))
;(truncate 12.34) -> 12
(define truncate int)
;(floor -12.34) -> -13
(define floor (lambda (n)
    (int
        (if (< n 0)
            (- n 1)
            n))))
;(ceiling 12.34) -> 13
(define ceiling (lambda (n) (- 0 (floor (- 0 n)))))
;(round 12.5) -> 13
(define round (lambda (n) (floor (+ n 0.5))))
;(mod 15 4) -> 3
(define mod (lambda (n m) (- n (* m (int (/ n m))))))
;(gcd 24 12) -> 12
(define gcd (lambda (n m)
    (if (eq? m 0)
        n
        (gcd m (mod n m)))))
;(lcm 24 12) -> 24
(define lcm (lambda (n m) (/ (* n m) (gcd n m))))
(define even? (lambda (n) (eq? (mod n 2) 0)))
(define odd? (lambda (n) (eq? (mod n 2) 1)))
(define length (lambda (t)
    (if t
        (+ 1 (length (cdr t)))
        0)))
;(nthcdr '(1 2 3 4) 2) -> [3, 4]
(define nthcdr (lambda (t n)
    (if (eq? n 0)
        t
        (nthcdr (cdr t) (- n 1)))))
;(nth '(1 2 3 4) 2) -> 3
(define nth (lambda (t n) (car (nthcdr t n))))
;(reverse '(1 2 3 4)) -> [4, 3, 2, 1]
(define rev1 (lambda (r t)
        (if t
            (rev1 (cons (car t) r) (cdr t))
            r)))
(define reverse (lambda (t) (rev1 nil t)))
;(member 2 '(1 2 3 4)) -> True
(define member (lambda (x t)
    (cond ((null? t) #f)
          ((equal? x (car t)) #t)
          (#t (member x (cdr t)))
    )))

;(foldr #'+ 0 '(1 2 3 4 5)) -> 15，计算 1 + (2 + (3 + (4 + (5 + 0))))
;(foldr #'list 0 '(1 2 3 4 5)) -> [1, [2, [3, [4, [5, 0]]]]]
(define foldr (lambda (f x t)
    (if t
        (f (car t) (foldr f x (cdr t)))
        x)))

;(foldl #'+ 0 '(1 2 3 4 5)) -> 15，计算 (((((0 + 1) + 2) + 3) + 4) + 5)
;(foldl #'list 0 '(1 2 3 4 5)) -> [5, [4, [3, [2, [1, 0]]]]]
(define foldl (lambda (f x t)
    (if t
        (foldl f (f (car t) x) (cdr t))
        x)))

;(filter odd? '(1 2 3 4 5)) -> [1, 3, 5]
(define filter (lambda (f t)
    (if t
        (if (f (car t))
            (cons (car t) (filter f (cdr t)))
            (filter f (cdr t)))
        nil)))
;(all? odd? '(1 2 3 4 5)) -> False
(define all? (lambda (f t)
    (if t
        (and
            (f (car t))
            (all? f (cdr t)))
        #t)))
;(any? odd? '(1 2 3 4 5)) -> True
(define any? (lambda (f t)
    (if t
        (or
            (f (car t))
            (any? f (cdr t)))
        nil)))

;(zip '(1 2 3) '(a b c))                             ;-> [(1, 'a'), (2, 'b'), (3, 'c')]
;((lambda (&rest x)   (list x))      1 2 3)          ;-> [[1, 2, 3]]
;((lambda (&rest x)   (list . x))    1 2 3)          ;-> [1, 2, 3]
;((lambda (f &rest x) (list f x))    1 2 3)          ;-> [1, [2, 3]]
;((lambda (f &rest x) (list f . x))  1 2 3)          ;-> [1, 2, 3]
;(map #'(lambda (x) (* x x)) '(1 2 3 4 5))        ;-> [1, 4, 9, 16, 25]
;(map #'(lambda (x y) (+ x y)) '(1 2 3) '(4 5 6)) ;-> [5, 7, 9]
;(map #'+ '(1 2 3) '(4 5 6))                      ;-> [5, 7, 9]
;(map #'fib '(1 2 3 4 5 6 7 8 9 10))              ;-> [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

;(seq 1 5) -> [1, 2, 3, 4]
(define seq (lambda (n m)
    (if (< n m)
        (cons n (seq (+ n 1) m))
        nil)))
;(seqby 1 10 2) -> [1, 3, 5, 7, 9]
(define seqby (lambda (n m k)
    (if (< 0 (* k (- m n)))
        (cons n (seqby (+ n k) m k))
        nil)))
;(range 1 5) -> [1, 2, 3, 4]
;(range 1 5 2) -> [1, 3]
(define range (lambda (n m &rest args)
    (if args
        (seqby n m (car args)) 
        (seq n m))))

(define curry (lambda (f x) (lambda (args) (f x . args))))
; (define add (lambda (a b) (+ a b)))
; ((curry add 3) 7) -> 10

; Y combinator
(define Y (lambda (f) (lambda (args) ((f (Y f)) . args))))
; (defun my-sqrt (n)
;     ((Y (lambda (f) (lambda (x)
;        (let* ((y (- x (/ (- x (/ n x)) 2))))
;            (if (= x y)
;                x
;                (f y))))))
;      n))
; (my-sqrt 2) -> 1.414213562373095
(defmacro dolist (x &rest args)   
    (let* ((var (lambda (x) (str+ '_ (car x)))))
          `(let*  ((,(var x) ,(car (cdr x))))
                    (while ,(var x)
                        (set! ,(car x) (car ,(var x)))
                        (set! ,(var x) (cdr ,(var x)))
                        ,@args
                    ))
))

(defmacro push (s var)
      `(set! ,var (cons ,s ,var)))

(defmacro pop (var)
      `(let* ((_ (car ,var)))
             (set! ,var (cdr ,var))
              _))

(define *stack* (lambda (n)
    (lambda (&rest m)
        (cond ((= :value (car m)) n) 
              ((= :clear (car m)) (set! n nil)) 
              (#t (set! n (cons (car m) n))))
        )))

(define collect (*stack* nil))
(defmacro for (para &rest args)
    (let* ((*_stack* (lambda (n)
                (lambda (&rest m)
                    (cond ((= :value (car m)) n) 
                          ((= :clear (car m)) (set! n nil))
                          (#t (set! n (cons (car m) n)))) 
                    )))
           (_collect (*_stack* nil))
           (_env (env+ '_collect _collect))    
           (_for (lambda (x &rest y)
                (if (cdr x)
                    `(dolist ,(car x) ,(_for (cdr x) . y))
                    (if (= 2 (length y))
                        `(dolist ,(car x) (if ,(car y) (_collect ,(car (cdr y)))))
                        `(dolist ,(car x) (_collect ,(car y)))               
                    )))))
          (_collect :clear)
          (eval (_for para . args) _env)
          `(quote ,(_collect :value))
    )
)

(defmacro ffor (var lst body)
    `(map (lambda (,var) ,body) ,lst))
; (ffor y '(0 1 2) 
;     (ffor x '(0 1 2)  (list x y))
; )
; [[[0, 0], [1, 0], [2, 0]], [[0, 1], [1, 1], [2, 1]], [[0, 2], [1, 2], [2, 2]]]

(defmacro when (x &rest args) `(if (not ,x) nil (begin ,@args)) )
; (when #t  (print 1) (print 2) (print 3)) ; => 1 2 3
; (when #f  (print 1) (print 2) (print 3)) ; => []
; (when 'a  (print 1) (print 2) (print 3)) ; => 1 2 3
; (when nil (print 1) (print 2) (print 3)) ; => []
(defmacro unless (x &rest args) `(if ,x nil (begin ,@args)))
; (unless #t (print 1) (print 2) (print 3)) ; => []
; (unless #f (print 1) (print 2) (print 3)) ; => 1 2 3


; (map (lambda (y)
;     (map (lambda (x) (list x y)) `(0 1 2)))
;     `(a b c))
;;[[[0, 'a'], [1, 'a'], [2, 'a']], [[0, 'b'], [1, 'b'], [2, 'b']], [[0, 'c'], [1, 'c'], [2, 'c']]]

(defun mapcar (fn lis)
  (when lis
    (cons (fn (car lis))
	  (mapcar fn (cdr lis)))))

(defun cadr (x)
   (car (cdr x)))

(defmacro let (binds &rest body)
  (define vars (map car binds))
  (define prms (map #'(lambda (x) (car (cdr x))) binds))
  `((lambda ,vars ,@body) ,@prms))

; > (let ((x 1) (y 2)) 
; >    (+ x y))
; 3