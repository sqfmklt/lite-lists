;; Copyright 2012 sqfmklt
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;; 
;;     (1) Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer. 
;; 
;;     (2) Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.  
;;     
;;     (3)The name of the author may not be used to
;;     endorse or promote products derived from this software without
;;     specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


;; This file tests the main code transformation macro and also serves
;; as documentation by example code.

(defpackage :lite-lists-test
  (:use :common-lisp :lite-lists))

(in-package :lite-lists-test)


(wrap-code


(defun naive-fibs-st (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (naive-fibs (- n 1)) (naive-fibs (- n 2))))))

(defun naive-fibs (n)
  $ cond (= n 0 ~ 0)
         (= n 1 ~ 1)
         $ t $ + (naive-fibs $ - n 1) $ naive-fibs $ - n 2)

(defun better-fibs-st (n)
  (named-let loopi ((i 1) (a 0) (b 1))
    (if (= i n)
      b
      (loopi (+ i 1) b (+ a b)))))

(defun better-fibs (n)
  $ named-let loopi ($ i 1 % a 0 % b 1)
    $ if (= i n)
      b
      $ loopi (+ i 1) b $ + a b)

(defun range-st (i j)
  (named-let loopi ((i i) (acc '()))
    (if (>= i j)
      (reverse acc)
      (loopi (+ i 1) (cons i acc)))))

(defun range (i j)
  $ named-let loopi ($ i i % acc '())
    $ if (>= i j)
      (reverse acc)
      $ loopi (+ i 1) $ cons i acc)

(defun main ()
  $ let* ((values $ range 1 15)
          (gen-results $ lambda (name fn) $ list name $ map 'list fn values)
          (names '(naive-fibs-st naive-fibs better-fibs-st better-fibs))
          (functions $ list #'naive-fibs-st #'naive-fibs #'better-fibs-st #'better-fibs)
          $ report $ map 'list gen-results names functions)
      (print report))
      

$ main

);;wrap-code
