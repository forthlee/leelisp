# lispLee

<pre>
> python test.py
********************************************* : 0 out of 79 tests fail.
********************************************* : 0 out of 64 tests fail.

> python lispLee.py
lispLee> (dis fib)
fib <lambda> ('x',)
(GET('x'), CONST(3), CALL('<',2), JMP1(10), 
 GET('x'), CONST(1), CALL('-',2), CALL('fib',1), 
 GET('x'), CONST(2), CALL('-',2), CALL('fib',1), 
 CALL('+',2), JUMP(1), CONST(1))

lispLee> (print "fib(10)=" (fib 10))
fib(10)= 55

lispLee> (define stack nil)
lispLee> (push 1 stack)
lispLee> (push 2 stack)
lispLee> (push 3 stack)
lispLee> stack
[3, 2, 1]
lispLee> (pop stack)
3

lispLee> (let* ((stack nil))
      (dolist (x '(1 2 3))
            (dolist (y '(a b c))
                (push (list x y) stack)
            )
       )
       stack
      )
[[3, 'c'], [3, 'b'], [3, 'a'], [2, 'c'], [2, 'b'], [2, 'a'], [1, 'c'], [1, 'b'], [1, 'a']]

lispLee> (map (lambda (m) (map (lambda (n) (list m n)) (seq 1 4))) (seq 1 4))
[[[1, 1], [1, 2], [1, 3]], [[2, 1], [2, 2], [2, 3]], [[3, 1], [3, 2], [3, 3]]]

lispLee> (for ((i '(1 2)) (j '(3 4))) (eq? 5 (+ i j)) (list i j)))
[[2, 3], [1, 4]]
</pre>

Bytecode compilers and interpreters<br>
https://bernsteinbear.com/blog/bytecode-interpreters/
