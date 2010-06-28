;; Created 23 Jun 2010 by Torbjorn Tornkvist <tobbe@tornkvist.org>
;; Inspired by the Haskell prelude.

;; erl -pa ./ebin -noshell -noinput -s lfe_boot start
;; > (slurp "src/prelude.lfe")

(defmodule prelude
  (export (null? 1))
  (export (.. 2))
  (export (reverse 1))
  (export (drop 2))
  (export (take 2))
  (export (rotate 2))
  (export (splitAt 2))
  (export (span 2))
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

;;
;; > (null? '()).            
;; true
;;
(defun null? (lst)
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
;; > (drop 2 '(1 2 3 4 5 6)) 
;; (3 4 5 6)
;;
(defun drop (n lst)
  (if (=< n 0) lst (drop (- n 1) (cdr lst))))

;;
;; > (take 2 '(1 2 3 4 5 6)) 
;; (1 2)
;;
(defun take (n lst)
  (if (=< n 0) '() (cons (car lst) (take (- n 1) (cdr lst)))))

;;
;; > (rotate 2 '(1 2 3 4 5 6))
;; (3 4 5 6 1 2)
;;
(defun rotate (n lst)
  (: lists append (drop n lst) (take n lst)))

;;
;; > (splitAt 5 (.. 1 12))
;; #((1 2 3 4 5) (6 7 8 9 10 11 12))
;;
(defun splitAt (n lst)
  (tuple (take n lst) (drop n lst)))

;;
;; > (span (lambda (x) (< x 7)) (.. 1 12))
;; #((1 2 3 4 5 6) (7 8 9 10 11 12))
;;
(defun span (p lst)
  (tuple (takeWhile p lst) (dropWhile p lst)))

;;
;; > (.. 1 10)               
;; (1 2 3 4 5 6 7 8 9 10)
;;
(defun .. (from to)
  (: lists seq from to))

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

