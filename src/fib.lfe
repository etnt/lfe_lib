(defmodule fib
  (export (start 0)))

(defmacro m (x)
  `(+ 100 ,x))

(defun start () 
  (: io format '"~p~n" (list (m (+ 1 3))))
  (: io format '"~p~n" (list (m (+ 1 2)))))
