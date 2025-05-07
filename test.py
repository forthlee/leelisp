from __future__ import print_function, division

lis_tests = [
    ("(quote (testing 1 (2.0) -3.14e159))", ['testing', 1, [2.0], -3.14e159]),
    ("(+ 2 2)", 4),
    ("(+ (* 2 100) (* 1 10))", 210),
    ("(if (> 6 5) (+ 1 1) (+ 2 2))", 2),
    ("(if (< 6 5) (+ 1 1) (+ 2 2))", 4),
    ("(define xx 3)", None), 
    ("xx", 3), 
    ("""(define xx "test ok")""", None), 
    ("xx", "test ok"), 
    ("""(evalsrc "(+ 1 2)")""", 3),
    ("""(eval `(1 ,@(2 3)) env)""", [1, 2, 3]),
    ("(begin (define xx 1) (set! xx (+ xx 1)) (+ xx 1))", 3),
    ("(begin (define xx '(1 (2 3) 4)) (setf xx (list 1 1) 9) xx)", [1, [2, 9], 4]),
    ("((lambda (x) x) 1)", 1),
    ("((lambda (&rest x) x) 1 2 3)", [1, 2, 3]),
    ("((lambda (x) (+ x x)) 5)", 10),
    ("(define twice (lambda (x) (* 2 x)))", None), 
    ("(twice 5)", 10),
    ("(define compose (lambda (f g) (lambda (x) (f (g x)))))", None),
    ("((compose list twice) 5)", [10]),
    ("(define repeat (lambda (f) (compose f f)))", None),
    ("((repeat twice) 5)", 20), 
    ("((repeat (repeat twice)) 5)", 80),
    ("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", None),
    ("(fact 3)", 6),
    ("(fact 50)", 30414093201713378043612608166064768844377641568960512000000000000),
    ("(define abs (lambda (n) ((if (> n 0) + -) 0 n)))", None),
    ("(list (abs -3) (abs 0) (abs 3))", [3,0,3]),
    ("(= (list #t #f nil) '(#t #f nil))", False),
    ("(= (list 1 2 3) '(1 2 3))", True),
    ("""(define combine (lambda (f) (lambda (x y)
            (if (null? x) (quote ())
                (f (list (car x) (car y))
                    ((combine f) (cdr x) (cdr y)))))))""", None),
    ("(list 1 2 3 4)", [1,2,3,4]),
    ("(cons (list 1 2 3 4) (list 5 6 7 8))", [[1,2,3,4], 5,6,7,8]),
    ("((combine cons) (list 1 2 3 4) (list 5 6 7 8))", [[1,5], [2,6], [3,7], [4,8]]),
    ("(define my-zip (combine cons))", None),
    ("(my-zip (list 1 2 3) (list 'a 'b 'c))", [[1,'a'], [2,'b'], [3,'c']]),
    ("(zip '(1 2 3) '(a b c))", [[1,'a'], [2,'b'], [3,'c']]),
    ("""(define riff-shuffle (lambda (deck) (begin
        (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
        (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
        (define mid (lambda (seq) (/ (length seq) 2)))
        ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))""", None),
    ("(riff-shuffle (list 1 2 3 4 5 6 7 8))", [1,5,2,6,3,7,4,8]),
    ("((repeat riff-shuffle) (list 1 2 3 4 5 6 7 8))", [1,3,5,7,2,4,6,8]),
    ("(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))", [1,2,3,4,5,6,7,8]),
    ("(list 1 . (list 2 3))", [1, 2, 3]),
    ("(list 'list (list 1 2)))", ['list', [1, 2]]),
    ("(list 'if . (list 1 2)))",  ['if', 1, 2] ),
    ("(list 'if 1 2))", ['if', 1, 2]),
    ("`(list ,(list 1 2)))",  ['list', [1, 2]]),
    ("`(list ,@(list 1 2)))", ['list', 1, 2]),
    ("`(if 1 2))", ['if', 1, 2]),
    ("(defun o1 (x y) (list x y))", None),
    ("(defun o2 (x y) (list x . y))", None),
    ("(defun o3 (x &rest y) (list x y))", None),
    ("(defun o4 (x &rest y) (list x . y))", None),
    ("(o1 1 (list 2 3)))", [1, [2, 3]]),
    ("(o2 1 (list 2 3)))", [1, 2, 3]),
    ("(o3 1 (list 2 3)))", [1, [[2, 3]]]),
    ("(o4 1 (list 2 3)))", [1, [2, 3]]),
    ("(o1 1 . (list 2 3)))", AssertionError),
    ("(o2 1 . (list 2 3)))", AssertionError),
    ("(o3 1 . (list 2 3)))", [1, [2, 3]]),
    ("(o4 1 . (list 2 3)))", [1, 2, 3]),
    ("(defmacro q1 (x y) `(list ,x ,y))", None),
    ("(defmacro q2 (x y) `(list ,x ,@y))", None),
    ("(defmacro q3 (x &rest y) `(list ,x ,y))", None),
    ("(defmacro q4 (x &rest y) `(list ,x ,@y))", None), 
    ("(q1 1 (list 2 3))", [1, [2, 3]]),
    ("(q3 1 (list 2 3))", AssertionError),
    ("(q4 1 (list 2 3))", [1, [2, 3]]),
    ("(q1 1 . (list 2 3))", [1, 2, 3]),
    ("(q3 1 . (list 2 3))", AssertionError),
    ("(q4 1 . (list 2 3))", [1, 2, 3]),
    ("((lambda (&rest x) (list x)) 1 2 3)", [[1, 2, 3]]),
    ("((lambda (&rest x) (list . x)) 1 2 3)", [1, 2, 3]),
    ("((lambda (f &rest x) (list f x)) 1 2 3)", [1, [2, 3]]),
    ("((lambda (f &rest x) (list f . x)) 1 2 3)", [1, 2, 3]),    
    ("(mapcar (lambda (x) (* x x)) '(1 2 3 4 5))", [1, 4, 9, 16, 25]),
    ("(map  #'(lambda (x) (* x x)) '(1 2 3 4 5))", [1, 4, 9, 16, 25]),
    ("(map #'(lambda (x y) (+ x y)) '(1 2 3) '(4 5 6))", [5, 7, 9]),
    ("(map #'+ '(1 2 3) '(4 5 6))", [5, 7, 9]),
    ("(map #'fib '(1 2 3 4 5 6 7 8 9 10))", [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]),
    ]

lispy_tests = [
    ("()", []), 
    ("(set! x)", AssertionError),
    ("(define 3 4)", AssertionError),
    ("(quote 1 2)", AssertionError),
    ("(if 1 2 3 4)", AssertionError),
    ("(lambda 3 3)", AssertionError), 
    ("(lambda (x))", AssertionError),
    ("(defun twice (x) (* 2 x))", None), 
    ("(twice 2)", 4),
    ("(twice 2 2)", AssertionError),
    ("((lambda (x) x) '(1 2 3 4))", [1,2,3,4]),
    ("((lambda (&rest x) x) '(1 2 3 4))", [[1,2,3,4]]),
    ("(define lyst (lambda (&rest x) x))", None),
    ("(lyst 1 2 3 (+ 2 2))", [1,2,3,4]),
    ("(if 1 2 3)", 2),
    ("(if (= 3 4) 2)", None),
    ("(cond ((= 3 4) 2))", None),
    ("""(define account (lambda (x) ; 閉包
            (lambda (y) (begin 
                (set! x (+ x y))    ; set! 可修改父階變數
                x
            ))))""", None), 
    ("(define a1 (account 100))", None),
    ("(a1 0)",  100), 
    ("(a1 10)", 110), 
    ("(a1 10)", 120),
    ("(define collect1 (*stack* nil))", None),
    ("(define collect2 (*stack* nil))", None),
    ("(collect1 10)", None),
    ("(collect2 20)", None),
    ("(collect1 30)", None),
    ("(collect2 40)", None),
    ("(collect1 :value)", [30, 10]),
    ("(collect2 :value)", [40, 20]),
    ("(collect1 :clear)", None),
    ("(collect1 :value)", []),
    ("(for ((i '(1 2)) (j '(3 4))) (list i j))", [[2, 4], [2, 3], [1, 4], [1, 3]]),
    ("(for ((i '(1 2)) (j '(3 4))) (eq? 5 (+ i j)) (list i j)))", [[2, 3], [1, 4]]),
    ("""(ffor x '(0 1) 
            (ffor y '(a b)  (list x y)))""", [[[0, 'a'], [0, 'b']], [[1, 'a'], [1, 'b']]]), # 類collect
    ("""(define newton (lambda (guess func derivative epsilon)
            (begin
                (define guess2 (- guess (/ (func guess) (derivative guess))))
                (if (< (abs (- guess guess2)) epsilon) 
                    guess2
                    (newton guess2 func derivative epsilon)))))""", None),
    ("""(defun square-root (a)
            (newton 1 (lambda (x) (- (* x x) a)) (lambda (x) (* 2 x)) 0.00000001))""", None),
    ("(> (square-root 200) 14.14213)", True),
    ("(< (square-root 200) 14.14215)", True),
    ("""(defun sum-squares-range (start end)
            (begin 
                (defun sumsq-acc (start end acc)
                    (if (> start end) acc (sumsq-acc (+ start 1) end (+ (* start start) acc))))
                (sumsq-acc start end 0)
            ))""", None),
    ("(sum-squares-range 1 300)", 9045050),
    ("(call/cc (lambda (throw) (+ 5 (* 10 (throw 1))))) ;; throw", 1),
    ("(call/cc (lambda (throw) (+ 5 (* 10 1)))) ;; do not throw", 15),
    ("""(call/cc (lambda (throw)
         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3)))))))) ; 1 level""", 35),
    ("""(call/cc (lambda (throw)
         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3)))))))) ; 2 levels""", 3),
    ("""(call/cc (lambda (throw)
         (+ 5 (* 10 (call/cc (lambda (escape) (* 100 1))))))) ; 0 levels""", 1005),
    ("(let* ((a 1) (b 2)) (+ a b))", 3),
    ("(let* ((a 1) (b 2 3)) (+ a b))", AssertionError),
    ("(and (> 2 1) (> 2 3))", False),
    ("(define unless (macro (&rest args) `(cond ((not ,(car args)) (begin ,@(cdr args)))))) ; test `", None),
    ("(unless (= 2 (+ 1 1)) 2 3 4)", None),
    ("(unless (= 4 (+ 1 1)) 2 3 4)", 4),
    ("(quote x)", 'x'),
    ("(quote (1 2 three))", [1,2,'three']),
    ("'x", 'x'),
    ("'(one 2 3)", ['one',2,3]),
    ("(define L (list 1 2 3))", None),
    ("`(testing ,@L testing)", ['testing',1,2,3,  'testing']),
    ("`(testing ,L  testing)", ['testing',[1,2,3],'testing']),
    ("""'(1 ;test comments '
     ;skip this line
     2 ; more ; comments ; ) )
     3) ; final comment""", [1,2,3]),
    ("(define add (lambda (a b) (+ a b)))", None),
    ("((curry add 3) 7)", 10),
    ("""(defun my-sqrt (n)
         ((Y (lambda (f)
                 (lambda (x)
                     (let*
                         ((y (- x (/ (- x (/ n x)) 2))))
                         (if (= x y)
                             x
                             (f y))))))
          n))""", None),
    ("(my-sqrt 2)", 1.414213562373095),
    ]

def to_string(exp):
    "Convert a Python object back into a Lisp-readable string."
    return '('+' '.join(map(to_string, exp))+')' if isinstance(exp, list) else str(exp)

def test(tests, name=''):
    "For each (exp, expected) test case, see if eval(parse(exp)) == expected."
    fails = 0
    for (x, expected) in tests:
        try:
            result = eval(compile(ast(tokenize(x))), env)
            ok = (result == expected)
        except Exception as e:
            result = type(e)            
            ok = issubclass(expected, Exception) and isinstance(e, expected)           
        if not ok:
            fails += 1
            print('FAIL!', x, '=>', to_string(result), ';;Expected:', expected)
    print('%s %s: %d out of %d tests fail.' % ('*'*45, name, fails, len(tests)))

if __name__ == '__main__':
    from llisp import *
    test(lis_tests)
    test(lispy_tests)
