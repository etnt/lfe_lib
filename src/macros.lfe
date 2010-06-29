;; Created 23 Jun 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>

(defmodule macros
  (export (gensym 0))
  (export (touch 1))
  (export (append-to-file 1))
  (export (add1 1))
  (export (add2 1))
;  (export (add3 1))
  (export (foo 2))
  (export (foo 2))
 ; (export (bar 1))
;  (export (bar 2))
;  (export (oo 2))
;  (export (abc 1))
   (export (start 0))
  (import (from prelude (splitAt 2) (drop 2) (take 2)))
  )

;;
;; Poor mans gensym
;;  
(defun gensym ()
  (fletrec ((g (n)
               (let ((uniq? (++ '"_#G" (integer_to_list n))))
                 (try (progn
                        (: erlang list_to_existing_atom uniq?)
                        (g (+ n 1)))
                      (catch
                        ((tuple _ _ _) (: erlang list_to_atom uniq?)))))))
    (g 1)))

;;
;;
;;
(defmacro with-open-file
  (((fd fname . options) . body)
   (let ((|00| (if (== options ())
                 `(: file open ,fname (list ',fd))
                 `(: file open ,fname . ,options))))
     `(let (((tuple 'ok ,fd) ,|00|))
        (try (progn . ,body)
             (after
               (: file close ,fd)))))))

(defun touch (fname)
  (with-open-file 
   (fd fname)
   (: io format fd
      '"(~p): time is ~p~n" 
      (list fd (: erlang time)))
   ))

(defun append-to-file (fname)
  (with-open-file
   (out fname (list 'append))
   (: io format out 
      '"(~p): time is ~p~n" 
      (list out (: erlang time)))
   (: erlang error 'crash)
   ))

(defun read-line (fd)
  (: io get_line fd 'nil))

(defmacro dofile 
  (((var filename) . body)
   `(with-open-file 
     (read filename)
     (do ((,var (read-line read) (read-line read)))
         ((== 'false (: erlang is_list ,var)),var)
       ,@body))))


(defun add1 (x) (+ x 1))
(defun add2 (x) (+ x 2))
;(defun add3 (x) (+ x 3))

	 
(defun foo (x) (lambda (y) (foo x y)))
(defun foo (x y) (+ x y))

(defun start ()
  (macrolet ((f (args `(: io format ,@args))))
    (f '"Hello Macro ~p~n" '("hejsan"))))



	     
(defmacro curry (f args body)
  (let* ((def `'(defun ,f ,args ,body))
	 (defs `(cons ,def (curry-1 ,f ,args))))
    defs))


(defmacro curry-1 (f args)
  (let ((len (: erlang length args)))
    (cond ((== len 1) `'((defun ,f () (lambda ,args (,f ,@args)))))
	  (else
	   (let* ((args0 (take (- len 1) args))
		  (var (drop (- len 1) args))
		  (def `'(defun ,f ,args0 (lambda ,var (,f ,@args)))))
	     `(cons ,def (curry-1 ,f ,args0)))))))
	

;(curry add (x y) (+ x y))


