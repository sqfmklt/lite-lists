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



;; using fold
(require 'srfi-1)


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
(define (lite-lists:parse-delimiters lis is-delimiter)
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
(define (lite-lists:parse-beginnings lis is-delimiter)
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
(define (lite-lists:parse-endings lis is-delimiter)
  (let loop ((lis (reverse lis)) (acc '()))
    (cond ((null? lis) acc)
          ((is-delimiter (car lis)) (loop (cdr lis) (list acc)))
          (else (loop (cdr lis) (cons (car lis) acc))))))

;;;;;;;;;;;;;;
;; A map reduce like function used to factor the rec-parse-beginnings and rec-parse-endings functions.
;;
;; Creates a deep copy of obj where lis-mapper is recursively applied to all sub lists within obj.
;;
(define (lite-lists:deep-map lis-mapper obj)
  (if (list? obj)
    (lis-mapper (map (lambda (elem) (lite-lists:deep-map lis-mapper elem)) obj))
    obj))

;;;;;;;;;;;;;;
;; Recursively applies parse-beginnings to all sublists of obj. Acts as a syntax transformer
;; for the front collapsing list syntax extension.
;;
(define (lite-lists:rec-parse-beginnings obj is-delimiter)
  (lite-lists:deep-map
    (lambda (sub-lis)
      (lite-lists:parse-beginnings sub-lis is-delimiter))
    obj))

;;;;;;;;;;;;;;
;; Applies parse-ending recursively to all sublists within obj. This is the syntax transformer
;; function for the back list collapsing.
;;
(define (lite-lists:rec-parse-endings obj is-delimiter)
  (lite-lists:deep-map
    (lambda (sub-lis)
      (lite-lists:parse-endings sub-lis is-delimiter))
    obj))

;;;;;;;;;;;;;;
;; Similar to parse-delimeters.
;;
;; lis is a list, where some of its items may make is-delimeter true.
;;
;; outer-lis is a backwards results list. Parsed sublists of lis will
;;   be appended to the front of outer-lis in reverse order.
;;
;; The returned result maybe reversed, or used as a new outer-lis argument
;; in further calls to rev-parse-delimeters.
;;
(define (lite-lists:rev-parse-delimiters lis outer-lis is-delimiter)
  (let loop ((lis lis) (inner-acc '()) (outer-acc outer-lis))
    (cond ((null? lis) (cons (reverse inner-acc) outer-acc))
          ((is-delimiter (car lis)) (loop (cdr lis) '() (cons (reverse inner-acc) outer-acc)))
          (else (loop (cdr lis) (cons (car lis) inner-acc) outer-acc)))))
 
;;;;;;;;;;;;;;
;; Finds all level 1 deep lists within lis and splits them on items that make
;; is-delimeter true. The split components then reside in layer 0 of lis.
;; Items in layer 0 of lis that make is-delimeter true are not affected.
;;
;; Example: (sub-parse-delimeters '(1 2 (3 4 | 5 6 | 7 8) 9 (10 | 11)) (lambda (x) (eq? x '|)))
;;       -> (1 2 (3 4) (5 6) (7 8) 9 (10) (11))
;;
;; The purpose of this is to make | act like a text substitution macro for )(
;; This type of macro transformation was too confusing for me to use.
;;
(define (lite-lists:sub-parse-delimiters lis is-delimiter)
  (reverse (fold (lambda (elem acc)
                   (if (list? elem)
                     (lite-lists:rev-parse-delimiters elem acc is-delimiter)
                     (cons elem acc)))
                 '()
                 lis)))

;;;;;;;;;;;;;;
;; Applies sub-parse-delimeters recursively to all inner lists within lis.
;;
(define (lite-lists:rec-sub-parse-delimiters lis is-delimiter)
  (lite-lists:deep-map
    (lambda (sub-lis)
      (lite-lists:sub-parse-delimiters sub-lis is-delimiter))
    lis))


;; Macros:

;; The macros takes a marker symbol as an argument so that you can use other symbols besides ~ | and $.

(defmacro (lite-lists:with-delimiter-mark marker . body)
  (cons 'begin (lite-lists:rec-sub-parse-delimiters body (lambda (x) (eq? x marker)))))

(defmacro (lite-lists:with-end-mark marker . body)
  (cons 'begin (lite-lists:rec-parse-endings body (lambda (x) (eq? x marker)))))

(defmacro (lite-lists:with-begin-mark marker . body)
  (cons 'begin (lite-lists:rec-parse-beginnings body (lambda (x) (eq? x marker)))))


;; Some utility functions for using this syntax in a repl.

(define (lite-lists:map-reader fn reader)
  (lambda ()
    (fn (reader))))

(define (lite-lists:loop-eval-read reader printer)
  (printer (eval (reader)))
  (lite-lists:loop-eval-read reader printer))

(define (lite-lists:line-printer line)
  (display line) (newline) (display '>>))

(define (lite-lists:delimiter-reader marker reader)
  (lite-lists:map-reader (lambda (line)
                           (lite-lists:rec-sub-parse-delimiters line (lambda (x) (eq? x marker))))
                         reader))

(define (lite-lists:end-reader marker reader)
  (lite-lists:map-reader (lambda (line)
                           (lite-lists:rec-parse-endings line (lambda (x) (eq? x marker))))
                         reader))

(define (lite-lists:begin-reader marker reader)
  (lite-lists:map-reader (lambda (line)
                           (lite-lists:rec-parse-beginnings line (lambda (x) (eq? x marker))))
                         reader))

;; A print eval read loop that transforms expressions using the | ~ $ symbols for the delimiters.
(define (lite-lists:repl)
  (lite-lists:loop-eval-read (lite-lists:delimiter-reader '| (lite-lists:begin-reader '~ (lite-lists:end-reader '$ read)))
                             lite-lists:line-printer))


;; A macro that uses the | ~ $ symbols for the delimiters.
(defmacro (lite-lists:wrap-code . body)
  `(lite-lists:with-end-mark $
     (lite-lists:with-begin-mark ~
       (lite-lists:with-delimiter-mark |
         ,@body))))



