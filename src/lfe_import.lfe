;; http://gist.github.com/306323

(defmodule imp (export (bar 0)))

(eval-when-compile
  (defun import-function-1 (module function)
    ;(: io format '"~p~p~n" (list module function))
    (let ((mod-fun (list_to_atom
		    (: lists concat (list module '":" function)))))

     ; (: io format '"~p~n" (list mod-fun))

      (let ((bar `(defmacro ,mod-fun 
		    (()
		     `(: ,',module ,',function))
		    ((arg)
		     `(: ,',module ,',function ,arg))
		    ((arg . args)
		     `(: ,',module ,',function ,arg ,@args)))))
	;(: io format '"~p~n" (list bar))
	bar))))

(defmacro import-function (module function)
 (import-function-1 module function))

(defmacro import-module (module)
  ;(: io format '"~p~n" (list module))
  (let ((mod-symbols (call module 'module_info 'exports)))
   ; (: io format '"~p~n" (list mod-symbols))
    (let ((bar `(progn
		  ,@(:	lists 
			usort
		      (: lists map 
			(lambda (X)
			  (case X 
			    ((tuple fn-symbol _arity) 
			     (import-function-1 module fn-symbol))))
			mod-symbols)))))
    ;  (: io format '"~p~n" (list bar))
      bar)))

(import-module io)
(import-module math)
(defun bar () (io:format '"bar~p~n" (list (math:sin (math:pi)))))


; 
; If you look at test_macro.lfe

(defmacro let@
  (((vb . vbs) . b) `(let (,vb) (let@ ,vbs . ,b)))
  ((() . b) `(begin . ,b)))

(defsyntax let&
  ([(vb . vbs) . b] [let (vb) (let& vbs . b)])
  ([() . b] [begin . b]))