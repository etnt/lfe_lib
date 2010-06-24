;; Created 23 Jun 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>

(defmodule prelude
  (export (gensym 0))
  (export (touch 1))
  (export (append-to-file 1))
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

