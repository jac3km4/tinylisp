# tinylisp
Tiny lisp implementation in haskell with parsing combinators built from scratch in ~200 lines of code.

# usage
The default executable boots to REPL mode, which can be used as:

```
(defun compose (fa fb) (lambda (a) (fb (fa a))))
(setq f (compose (lambda (a) (+ a 10)) (lambda (a) (+ a 4))))
(println (f 10))
> 24
```
