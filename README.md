    (README
    
      (description
        This is a small scheme library that provides a syntax that helps reduce the stacking
        of parens in lisp code. It should theoretically work on any scheme implementation that
        supports r5rs slib. It has also been ported to r6rs scheme provided that the unportable
        define-macro form is provided.


      (examples
        ((scheme

          (define (factorial n)
            (letrec ((helper (lambda (n acc)
                               (if (= n 0)
                                 acc
                                 (helper (- n 1) (* n acc))))))
              (helper n 1)))

         );;scheme

         (scheme-with-extension

          (define (factorial n)
            $ letrec ($ helper $ lambda (n acc)
                          $ if (= n 0)
                              acc
                              $ helper (- n 1) $ * n acc)
                $ helper n 1)

         );;scheme-with-extension
        )

     
       ((scheme

         (define (split lis is-delimiter)
           (let loop ((lis lis) (inner-acc '()) (outer-acc '()))
             (cond ((null? lis)
                    (reverse (cons (reverse inner-acc) outer-acc)))
                   ((is-delimiter (car lis))
                    (loop (cdr lis) '() (cons (reverse inner-acc) outer-acc)))
                   (else
                    (loop (cdr lis) (cons (car lis) inner-acc) outer-acc)))))
        );;scheme

        (scheme-with-extension

         (define (split lis is-delimeter)
           $ let loop ($ lis lis % inner-acc '() % outer-acc '())
               $ cond ((null? lis)
                       $ reverse $ cons (reverse inner-acc) outer-acc)
                      ((is-delimeter $ car lis)
                       $ loop (cdr lis) '() $ cons (reverse inner-acc) outer-acc)
                      $ else
                        $ loop (cdr lis) (cons (car lis) inner-acc) outer-acc)

        );;scheme-with-extension
       )
      )

      (installation
    
        If you have a scheme implementation that supports slib \, you can use this script by doing the following
    
        (steps
          ((copy the script somewhere)
           ((for example) "/home/myname/somepath/lite-lists.scm"))
          ((create a file in your home directory called homecat \, with the following content)
    "
    (
      (lite-lists defmacro "/home/myname/somepath/lite-lists.scm")
    )
    "
          )
         ((you can try using it)
          (open a scheme prompt)
          (type (require 'lite-lists))
          (see test.scm for a sample program)))
    
    
        (references
          (For more information about slib \, look here (url "http://people.csail.mit.edu/jaffer/SLIB.html")))
    
        (comment
          You are free to use a different library system of course. It wouldn't be hard to port to a more specific
          module system offered by your scheme implementation. A version that uses portable r6rs will be
          coming soon \, and common lisp as well))
    
      (notes
    
        (premise
          (a common complaint about lisp and scheme is the amount of parentheses)
          (one quality I've noticed with lisp is that parentheses often stack up at the end of expressions)
          (I've also seen the apply operator \, $ \, in Haskell used to reduce parentheses)
          (so why not do the same in lisp ?)
          (The $ symbol now starts a new list \, which will end at the same place as the outer list)
          (for example \, (define (inner-product l1 l2) (apply + (map * l1 l2))) can be represented
           as (define (inner-product l1 l2) $ apply + $ map * l1 l2)))
    
        (extra
          (then in the spirit of experimentation \, I included two other symbols that are less useful and
           more likely to result in confusion)
          (~ acts like $ except that it folds the list up towards the front instead of the back)
          (so ((((1 2) 3 4) 5 6) 7 8) can be represented as (1 2 ~ 3 4 ~ 5 6 ~ 7 8))
          (% acts like a text substitution macro for ")(")
          (so (let ((a 1) (b 2) (c 3) (d 4)) (+ a b c d)) can be represented as (let ((a 1 % b 2 % c 3 % d 4)) (+ a b c d))  )
          (when % is used with $ \, let statements can sometimes be shorter (let ($ a 1 % b 2 % c 3 % d 4) $ + a b c d)  )
          (although the ability to do the above depends on the $ symbol being processed before %))
    
        (customization
          (the default macro is lite-lists:wrap-code)
          (this macro enables the 3 symbols)
          (if you want to use different symbols \, you can \, see the macros (with-delimiter-mark with-end-mark with-begin-mark)))
            
        (reference
          (see test.scm for a small sample program)))

      (license
        (modified bsd)))
     
