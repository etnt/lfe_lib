;; Created 23 Jun 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>

(defmodule prelude
  (export (reverse 1))
  (export (zip 2))
  (export (unzip 1))
  (export (map 2))
  (export (foldl 3))
  (export (foldr 3))
  (export (takeWhile 2))
  (export (dropWhile 2))
  (export (filter 2))
  (export (any 2))
  )

;;
;; A nice little macro for catching any thrown exception.
;;
(define-syntax catch-throw
  (macro 
   ((e) `(try ,e
	      (catch
		  ((tuple 'throw n o) n))))))


(defun null_p (lst)
  (if (== lst ()) 'true 'false))

;;
;; prelude:reverse("abcd").
;;
(defun reverse (list)
  (fletrec ((rev 
             ((() acc) acc)
             (((x . xs) acc) (rev xs (cons x acc)))))
    (rev list '())))

;;
;; prelude:zip([1,2,3,4],[a,b,c,d]).
;;
(defun zip 
  (((a . al) (b . bl)) (cons (tuple a b) (zip al bl)))
  ((() ()) ()))

;;
;; prelude:unzip(prelude:zip([1,2,3,4],[a,b,c,d])).
;;
(defun unzip (list)
  (fletrec ((unzip0
             ((() al bl) 
              (tuple (reverse al) (reverse bl)))
             ((xl al bl) 
              (let (((tuple a b) (car xl)))
                (unzip0 (cdr xl) (cons a al) (cons b bl))))))
    (unzip0 list () ())))

;;
;; prelude:map(fun(X) -> X*X end, [1,2,3,4,5]).
;;
(defun map 
  ((f (x . xs))
   (cons (funcall f x) (map f xs)))
  ((f ())
   ()))

;;
;; prelude:foldl(fun(X,Acc) -> X+Acc end, 0, [1,2,3,4]).
;;
(defun foldl
  ((f acc (x . xs))
   (foldl f (funcall f x acc) xs))
  ((f acc ())
   acc))

;;
;; prelude:foldr(fun(X,[]) -> [X]; (X,Acc) -> [X]++"."++Acc end, "", "abcd").
;;
(defun foldr
  ((f acc (x . xs))
   (funcall f x (foldr f acc xs)))
  ((f acc ())
   acc))

;;
;; prelude:takeWhile(fun(X) -> X < 3 end, [1,2,3,4,5]).
;;
(defun takeWhile 
  ((f (x . xs))
   (if (funcall f x)
      (cons x (takeWhile f xs))
     ()))
  ((f ())
   ()))

;;
;; prelude:dropWhile(fun(X) -> X < 3 end, [1,2,3,4,5]).
;;
(defun dropWhile 
  ((f (x . xs))
   (if (funcall f x)
      (dropWhile f xs)
     (cons x xs)))
  ((f ())
   ()))

;;
;; prelude:filter(fun(X) -> (X rem 2) == 0 end, [1,2,3,4,5,6,7]).
;;
(defun filter (f lst)
  (foldr 
   (lambda (x xs)
     (if (funcall f x)
	 (cons x xs)
       xs))
   '() lst))

;;
;; prelude:any(fun(X) -> (X rem 3) == 0 end, [1,2,3,4,5,6,7]).
;;
 (defun any (f lst)
   (catch-throw
    (foldl 
     (lambda (x b)
       (if (funcall f x)
	   (throw 'true)
	 b))
     'false lst)))

	 
  