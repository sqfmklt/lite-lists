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



;; This version should work with r6rs scheme implementations that also
;; support define-macro.


(library (lite-lists)
  (export parse-delimiters
          parse-beginnings
          parse-endings
          deep-map
          rec-parse-beginnings
          rec-parse-endings
          rev-parse-delimiters
          sub-parse-delimiters
          rec-sub-parse-delimiters
          with-delimiter-mark
          with-end-mark
          with-begin-mark
          map-reader
          loop-eval-read
          line-printer
          delimiter-reader
          end-reader
          begin-reader
          repl
          wrap-code)
  (import (rnrs)
          (rnrs eval)
          (only (srfi srfi-1) fold)
          (only (core syntax-case) define-macro)) ;; define-macro is not portable. only tested on ypsilon

;; Helper functions

;;;;;;;;;;;;;;
;; Not used, but may be useful for later modifications.
;; Takes a lis and returns a split on all items for which is-delimiter returns true.
;;
;; Example: (parse-delimiters '(1 2 | 3 4 | 5 6) (lambda (x) (eq? x '|)))
;;       -> ((1 2) (3 4) (5 6))
;;
;; sub-parse-delimiters ended up being used instead.
;;
(define (parse-delimiters lis is-delimiter)
  (let loop ((lis lis) (inner-acc '()) (outer-acc '()))
    (cond ((null? lis)
           (reverse (cons (reverse inner-acc) outer-acc)))
          ((is-delimiter (car lis))
           (loop (cdr lis)
                 '()
                 (cons (reverse inner-acc) outer-acc)))
          (else
           (loop (cdr lis)
                 (cons (car lis) inner-acc)
                 outer-acc)))))

;;;;;;;;;;;;;;
;; Collapses a list towards the front.
;;
;; Example: (parse-beginnings '(1 2 ~ 3 4 ~ 5 6) (lambda (x) (eq? x '~)))
;;       -> (((1 2) 3 4) 5 6)
;;
(define (parse-beginnings lis is-delimiter)
  (let loop ((lis lis) (acc '()))
    (cond ((null? lis) (reverse acc))
          ((is-delimiter (car lis)) (loop (cdr lis) (list (reverse acc))))
          (else (loop (cdr lis) (cons (car lis) acc))))))

;;;;;;;;;;;;;;
;; Collapses a list towards the back.
;;
;; Example: (parse-endings '(1 2 $ 3 4 $ 5 6) (lambda (x) (eq? x '$)))
;;       -> (1 2 (3 4 (5 6)))
;;
;; This one is the most practical syntax transformer, since most lisp code
;; has a lot of closing parens.
;;
(define (parse-endings lis is-delimiter)
  (let loop ((lis (reverse lis)) (acc '()))
    (cond ((null? lis) acc)
          ((is-delimiter (car lis)) (loop (cdr lis) (list acc)))
          (else (loop (cdr lis) (cons (car lis) acc))))))

;;;;;;;;;;;;;;
;; A map reduce like function used to factor the rec-parse-beginnings and rec-parse-endings functions.
;;
;; Creates a deep copy of obj where lis-mapper is recursively applied to all sub lists within obj.
;;
(define (deep-map lis-mapper obj)
  (if (list? obj)
    (lis-mapper (map (lambda (elem) (deep-map lis-mapper elem)) obj))
    obj))

;;;;;;;;;;;;;;
;; Recursively applies parse-beginnings to all sublists of obj. Acts as a syntax transformer
;; for the front collapsing list syntax extension.
;;
(define (rec-parse-beginnings obj is-delimiter)
  (deep-map
    (lambda (sub-lis)
      (parse-beginnings sub-lis is-delimiter))
    obj))

;;;;;;;;;;;;;;
;; Applies parse-ending recursively to all sublists within obj. This is the syntax transformer
;; function for the back list collapsing.
;;
(define (rec-parse-endings obj is-delimiter)
  (deep-map
    (lambda (sub-lis)
      (parse-endings sub-lis is-delimiter))
    obj))

;;;;;;;;;;;;;;
;; Similar to parse-delimiters.
;;
;; lis is a list, where some of its items may make is-delimiter true.
;;
;; outer-lis is a backwards results list. Parsed sublists of lis will
;;   be appended to the front of outer-lis in reverse order.
;;
;; The returned result maybe reversed, or used as a new outer-lis argument
;; in further calls to rev-parse-delimiters.
;;
(define (rev-parse-delimiters lis outer-lis is-delimiter)
  (let loop ((lis lis) (inner-acc '()) (outer-acc outer-lis))
    (cond ((null? lis) (cons (reverse inner-acc) outer-acc))
          ((is-delimiter (car lis)) (loop (cdr lis) '() (cons (reverse inner-acc) outer-acc)))
          (else (loop (cdr lis) (cons (car lis) inner-acc) outer-acc)))))
 
;;;;;;;;;;;;;;
;; Finds all level 1 deep lists within lis and splits them on items that make
;; is-delimiter true. The split components then reside in layer 0 of lis.
;; Items in layer 0 of lis that make is-delimiter true are not affected.
;;
;; Example: (sub-parse-delimiters '(1 2 (3 4 | 5 6 | 7 8) 9 (10 | 11)) (lambda (x) (eq? x '|)))
;;       -> (1 2 (3 4) (5 6) (7 8) 9 (10) (11))
;;
;; The purpose of this is to make | act like a text substitution macro for )(
;; This type of macro transformation was too confusing for me to use.
;;
(define (sub-parse-delimiters lis is-delimiter)
  (reverse (fold (lambda (elem acc)
                   (if (list? elem)
                     (rev-parse-delimiters elem acc is-delimiter)
                     (cons elem acc)))
                 '()
                 lis)))

;;;;;;;;;;;;;;
;; Applies sub-parse-delimiters recursively to all inner lists within lis.
;;
(define (rec-sub-parse-delimiters lis is-delimiter)
  (deep-map
    (lambda (sub-lis)
      (sub-parse-delimiters sub-lis is-delimiter))
    lis))


;; Macros:

;; The macros takes a marker symbol as an argument so that you can use other symbols besides ~ | and $.

(define-macro (with-delimiter-mark marker . body)
  (cons 'begin (rec-sub-parse-delimiters body (lambda (x) (eq? x marker)))))

(define-macro (with-end-mark marker . body)
  (cons 'begin (rec-parse-endings body (lambda (x) (eq? x marker)))))

(define-macro (with-begin-mark marker . body)
  (cons 'begin (rec-parse-beginnings body (lambda (x) (eq? x marker)))))


;; Some utility functions for using this syntax in a repl.

(define (map-reader fn reader)
  (lambda ()
    (fn (reader))))

(define (loop-eval-read reader printer)
  (printer (eval (reader)))
  (loop-eval-read reader printer))

(define (line-printer line)
  (display line) (newline) (display '>>))

(define (delimiter-reader marker reader)
  (map-reader (lambda (line)
                           (rec-sub-parse-delimiters line (lambda (x) (eq? x marker))))
                         reader))

(define (end-reader marker reader)
  (map-reader (lambda (line)
                           (rec-parse-endings line (lambda (x) (eq? x marker))))
                         reader))

(define (begin-reader marker reader)
  (map-reader (lambda (line)
                           (rec-parse-beginnings line (lambda (x) (eq? x marker))))
                         reader))

;; A print eval read loop that transforms expressions using the % ~ $ symbols for the delimiters.
(define (repl)
  (loop-eval-read (delimiter-reader '% (begin-reader '~ (end-reader '$ read)))
                             line-printer))


;; A macro that uses the % ~ $ symbols for the delimiters.
(define-macro (wrap-code . body)
  `(with-end-mark $
     (with-begin-mark ~
       (with-delimiter-mark %
         ,@body))))


);;library

