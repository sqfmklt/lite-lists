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



;; This version should work with common-lisp



(defpackage #:lite-lists
  (:use #:common-lisp)
  (:export #:named-let
           #:parse-delimiters
           #:parse-beginnings
           #:parse-endings
           #:deep-map
           #:rec-parse-beginnings
           #:rec-parse-endings
           #:rev-parse-delimiters
           #:sub-parse-delimiters
           #:rec-sub-parse-delimiters
           #:with-delimiter-mark
           #:with-end-mark
           #:with-begin-mark
           #:map-reader
           #:loop-eval-read
           #:line-printer
           #:delimiter-reader
           #:end-reader
           #:begin-reader
           #:repl
           #:wrap-code))

(in-package #:lite-lists)


;; I was too lazy to port the loop code to lisp style,
;; so I created a macro to emulate scheme's named let.
;
(defmacro named-let (name bindings &rest body)
  (let ((args (map 'list #'car bindings))
        (arg-values (map 'list #'cadr bindings)))
    `(labels ((,name ,args ,@body))
       (,name ,@arg-values))))


;; Helper functions

;;;;;;;;;;;;;;
;; Not used, but may be useful for later modifications.
;; Takes a lis and returns a split on all items for which is-delimiter returns true.
;;
;; Example: (parse-delimiters '(1 2 | 3 4 | 5 6) (lambda (x) (eq x '|)))
;;       -> ((1 2) (3 4) (5 6))
;;
;; sub-parse-delimiters ended up being used instead.
;;
(defun parse-delimiters (lis is-delimiter)
  (named-let loopi ((lis lis) (inner-acc '()) (outer-acc '()))
    (cond ((null lis)
           (reverse (cons (reverse inner-acc) outer-acc)))
          ((funcall is-delimiter (car lis))
           (loopi (cdr lis)
                 '()
                 (cons (reverse inner-acc) outer-acc)))
          (t
           (loopi (cdr lis)
                 (cons (car lis) inner-acc)
                 outer-acc)))))


;;;;;;;;;;;;;;
;; Collapses a list towards the front.
;;
;; Example: (parse-beginnings '(1 2 ~ 3 4 ~ 5 6) (lambda (x) (eq x '~)))
;;       -> (((1 2) 3 4) 5 6)
;;
(defun parse-beginnings (lis is-delimiter)
  (named-let loopi ((lis lis) (acc '()))
    (cond ((null lis) (reverse acc))
          ((funcall is-delimiter (car lis)) (loopi (cdr lis) (list (reverse acc))))
          (t (loopi (cdr lis) (cons (car lis) acc))))))


;;;;;;;;;;;;;;
;; Collapses a list towards the back.
;;
;; Example: (parse-endings '(1 2 $ 3 4 $ 5 6) (lambda (x) (eq x '$)))
;;       -> (1 2 (3 4 (5 6)))
;;
;; This one is the most practical syntax transformer, since most lisp code
;; has a lot of closing parens.
;;
(defun parse-endings (lis is-delimiter)
  (named-let loopi ((lis (reverse lis)) (acc '()))
    (cond ((null lis) acc)
          ((funcall is-delimiter (car lis)) (loopi (cdr lis) (list acc)))
          (t (loopi (cdr lis) (cons (car lis) acc))))))


;;;;;;;;;;;;;;
;; A map reduce like function used to factor the rec-parse-beginnings and rec-parse-endings functions.
;;
;; Creates a deep copy of obj where lis-mapper is recursively applied to all sub lists within obj.
;;
(defun deep-map (lis-mapper obj)
  (if (listp obj)
    (funcall lis-mapper (map 'list (lambda (elem) (deep-map lis-mapper elem)) obj))
    obj))

;;;;;;;;;;;;;;
;; Recursively applies parse-beginnings to all sublists of obj. Acts as a syntax transformer
;; for the front collapsing list syntax extension.
;;
(defun rec-parse-beginnings (obj is-delimiter)
  (deep-map
    (lambda (sub-lis)
      (parse-beginnings sub-lis is-delimiter))
    obj))

;;;;;;;;;;;;;;
;; Applies parse-ending recursively to all sublists within obj. This is the syntax transformer
;; function for the back list collapsing.
;;
(defun rec-parse-endings (obj is-delimiter)
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
(defun rev-parse-delimiters (lis outer-lis is-delimiter)
  (named-let loopi ((lis lis) (inner-acc '()) (outer-acc outer-lis))
    (cond ((null lis) (cons (reverse inner-acc) outer-acc))
          ((funcall is-delimiter (car lis)) (loopi (cdr lis) '() (cons (reverse inner-acc) outer-acc)))
          (t (loopi (cdr lis) (cons (car lis) inner-acc) outer-acc)))))

 
;;;;;;;;;;;;;;
;; Finds all level 1 deep lists within lis and splits them on items that make
;; is-delimiter true. The split components then reside in layer 0 of lis.
;; Items in layer 0 of lis that make is-delimiter true are not affected.
;;
;; Example: (sub-parse-delimiters '(1 2 (3 4 | 5 6 | 7 8) 9 (10 | 11)) (lambda (x) (eq x '|)))
;;       -> (1 2 (3 4) (5 6) (7 8) 9 (10) (11))
;;
;; The purpose of this is to make | act like a text substitution macro for )(
;; This type of macro transformation was too confusing for me to use.
;;
(defun sub-parse-delimiters (lis is-delimiter)
  (reverse (reduce (lambda (acc elem)
                     (if (listp elem)
                       (rev-parse-delimiters elem acc is-delimiter)
                       (cons elem acc)))
                   lis
                   :initial-value '())))

;;;;;;;;;;;;;;
;; Applies sub-parse-delimiters recursively to all inner lists within lis.
;;
(defun rec-sub-parse-delimiters (lis is-delimiter)
  (deep-map
    (lambda (sub-lis)
      (sub-parse-delimiters sub-lis is-delimiter))
    lis))

;; Macros:

;; The macros takes a marker symbol as an argument so that you can use other symbols besides ~ | and $.

(defmacro with-delimiter-mark (marker &rest body)
  (cons 'progn (rec-sub-parse-delimiters body (lambda (x) (eq x marker)))))

(defmacro with-end-mark (marker &rest body)
  (cons 'progn (rec-parse-endings body (lambda (x) (eq x marker)))))

(defmacro with-begin-mark (marker &rest body)
  (cons 'progn (rec-parse-beginnings body (lambda (x) (eq x marker)))))

;; Some utility functions for using this syntax in a repl.
(defun map-reader (fn reader)
  (lambda ()
    (funcall fn (funcall reader))))

(defun loop-eval-read (reader printer)
  (loop (funcall printer (eval (funcall reader)))))

(defun line-printer (line)
  (print line))

(defun delimiter-reader (marker reader)
  (map-reader (lambda (line)
                (rec-sub-parse-delimiters line (lambda (x) (eq x marker))))
              reader))

(defun end-reader (marker reader)
  (map-reader (lambda (line)
                (rec-parse-endings line (lambda (x) (eq x marker))))
              reader))

(defun begin-reader (marker reader)
  (map-reader (lambda (line)
                (rec-parse-beginnings line (lambda (x) (eq x marker))))
              reader))

;; A print eval read loop that transforms expressions using the % ~ $ symbols for the delimiters.
(defun repl ()
  (loop-eval-read (delimiter-reader '% (begin-reader '~ (end-reader '$ #'read)))
                  #'line-printer))

;; A macro that uses the % ~ $ symbols for the delimiters.
;; Had to use intern to create symbols in the same package as where
;; the macro is expanded.
(defmacro wrap-code (&rest body)
  `(with-end-mark ,(intern "$")
     (with-begin-mark ,(intern "~")
       (with-delimiter-mark ,(intern "%")
         ,@body))))


