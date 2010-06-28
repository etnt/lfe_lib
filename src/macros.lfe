;; Created 23 Jun 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>

(defmodule macros
  (export (gensym 0))
  (export (touch 1))
  (export (append-to-file 1))
  (export (add1 1))
  (export (add2 1))
  (export (add3 1))
  (export (foo 1))
  (export (foo 2))
  (export (bar 1))
  (export (bar 2))
;  (export (oo 2))
;  (export (abc 1))
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
(defun add3 (x) (+ x 3))

	 
(defun foo (x) (lambda (y) (foo x y)))
(defun foo (x y) (+ x y))

(defmacro curry (f)
  `(defun ,f (x) (lambda (y) (,f x y))))

(defun bar (x y) (+ x y))

(curry bar)

;(defmacro splice (lst)
;  `,@lst)

;(defun splice
;  ((defun add (x y) (+ x y))
;   (defun sub (x y) (- x y))))

;; (o f g) ==> (lambda (x) (f (g x))) 

;(eval-when-compile
;  (defun oo 
;    (((x . xs) v)
;     (: io format "hej~n")
;     (list x (oo xs v)))
;    (((x) v) (list x v))))

;(eval-when-compile
;  (defun oo (fs v)
;    (let (((x . xs) (: lists reverse fs)))
;      (: lists foldl (lambda (e acc) (list e acc)) (list x v) xs))))

;  (((x) v) 
;   (list x v))
;  (((x . xs) v)
;   (let ((z (oo xs v)))
;     (list x (list z)))))

; (o '(foo bar))
; (lambda (z) (foo (bar z)))

;(defmacro o (fs v)
;   (let* (((x . xs) (: lists reverse fs))
;	  (os (: lists foldl (lambda (e acc) (list e acc)) (list x v) xs)))
;     os))

;(defun abc (x)
;  (let ((f (o '(add1 add2 add3) x)))
;    (funcall f x)))

