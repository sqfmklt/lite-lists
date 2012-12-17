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

(import (lite-lists))


(wrap-code


(define (naive-fibs-st n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (naive-fibs (- n 1)) (naive-fibs (- n 2))))))

(define (naive-fibs n)
  $ cond (= n 0 ~ 0)
         (= n 1 ~ 1)
         $ else $ + (naive-fibs $ - n 1) $ naive-fibs $ - n 2)

(define (better-fibs-st n)
  (let loop ((i 1) (a 0) (b 1))
    (if (= i n)
      b
      (loop (+ i 1) b (+ a b)))))

(define (better-fibs n)
  $ let loop ($ i 1 % a 0 % b 1)
    $ if (= i n)
      b
      $ loop (+ i 1) b $ + a b)

(define (range-st i j)
  (let loop ((i i) (acc '()))
    (if (>= i j)
      (reverse acc)
      (loop (+ i 1) (cons i acc)))))

(define (range i j)
  $ let loop ($ i i % acc '())
    $ if (>= i j)
      (reverse acc)
      $ loop (+ i 1) $ cons i acc)

(define (main)
  $ let* ((values $ range 1 15)
          (gen-results $ lambda (name fn) $ list name $ map fn values)
          (names '(naive-fibs-st naive-fibs better-fibs-st better-fibs))
          (functions $ list naive-fibs-st naive-fibs better-fibs-st better-fibs)
          $ report $ map gen-results names functions)
      (display report)
      $ newline)
      

$ main

);;wrap-code
