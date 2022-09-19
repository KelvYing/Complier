#lang racket
;(require "Q8.rkt")
;(require "primpl.rkt")
(define ctr -3)

;returns the list of 'vars
(define (setting-vars lst)
  (match lst
    [(list-rest a b .....) (if (symbol? a) (if (symbol=? a 'vars) b #f) #f)]
    [else #f]))
;change the given list of 'vars to data, make sure to add _ in front of them
(define (set-datas lst result)
  (cond
    [(empty? lst) result]
    [else (set-datas (cdr lst) (cons (list 'data (caar lst) (cadar lst)) result) )]))
;helper function to add and pop elements in stack
(define (list-help sym)
  (cons (list sym (list -2 'SP) (list -2 'SP) (list -1 'SP)) (cons (list 'sub 'SP 'SP 1) (cons (list 'move (list 0 'SP) 0) empty))))
;functions to append sym
(define (symnumapp sym1 num)
  (string->symbol (string-append "_"(symbol->string sym1) (number->string num))))
(define (symapp sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))
;helper function to loop through and parsing a list
(define (loop-parse lst result)
  (cond
    [(empty? lst) result]
    [else (loop-parse (cdr lst) (append result (main-parse (car lst))))]))
    
;main parsing function
(define (main-parse exp)
  (match exp   ;Start with aexp
    [`(+,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'add))]
    [`(-,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'sub))]
    [`(*,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'mul))]
    [`(div,aexp1,aexp2)(append (main-parse aexp1) (main-parse aexp2) (list-help 'div))]
    [`(mod,aexp1,aexp2)(append (main-parse aexp1) (main-parse aexp2) (list-help 'mod))]
     ;now bexps
    [`(=,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'equal))]
    [`(>,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'gt))]
    [`(<,aexp1,aexp2)  (append (main-parse aexp1) (main-parse aexp2) (list-help 'lt))]
    [`(>=,aexp1,aexp2) (append (main-parse aexp1) (main-parse aexp2) (list-help 'ge))]
    [`(<=,aexp1,aexp2) (append (main-parse aexp1) (main-parse aexp2) (list-help 'le))]
    [`(and,bexp1,bexp2)(append (main-parse bexp1) (main-parse bexp2) (list-help 'land))]
    [`(or,bexp1,bexp2) (append (main-parse bexp1) (main-parse bexp2) (list-help 'lor))]
    [`(not,bexp)       (append (main-parse bexp)(cons (list 'lnot (list -1 'SP)(list -1 'SP))empty))]
    [`true           (cons (list 'move (list 0 'SP) #t ) (cons (list 'add 'SP 'SP 1)empty))]
    [`false          (cons (list 'move (list 0 'SP) #f ) (cons (list 'add 'SP 'SP 1)empty))]
    ;now stmts
    [`(set,var,aexp)   (append (main-parse aexp)(cons (list 'move var (list -1 'SP))(cons (list 'sub 'SP 'SP 1)(cons (list 'move (list 0 'SP) 0) empty))))]
    [`(print, val)     (if (string? val)
                           (cons (list 'print-string val) empty)
                           (append (main-parse val) (cons (list 'print-val (list -1 'SP)) (cons (list 'sub 'SP 'SP 1) (cons (list 'move (list 0 'SP) 0) empty)))))]
    [`(skip) empty]
    [`(seq ,stmt1 ,stmt2 ...)    (loop-parse (cdr exp) empty)]
    [`(iif, exp, stmt1,stmt2)(begin (set! ctr (+ 3 ctr))
                                    (define label1(symnumapp 'LABEL ctr)) 
                                    (define label2(symnumapp 'LABEL (+ 1 ctr)))
                                    (define label3 (symnumapp 'LABEL (+ 2 ctr)))
                                    (append (main-parse exp)(cons(list 'branch (list -1 'SP) label1)
                                                                 (cons (list 'sub 'SP 'SP 1)
                                                                                      (cons(list 'jump label2)
                                                                                           (cons (list 'label label1)
                                                                                                 (cons (list 'sub 'SP 'SP 1)
                                                                                                 (append (main-parse stmt1)
                                                                                                       (cons (list 'jump label3)
                                                                                                             (cons (list 'label label2)
                                                                                                                   (append (main-parse stmt2)
                                                                                                                         (cons (list 'label label3)
                                                                                                                               empty
                                                                                                                               ))))))))))))]
    [`(while ,a ,b ...) (begin (set! ctr (+ 3 ctr))
                               (define label1(symnumapp 'LABEL ctr)) 
                               (define label2(symnumapp 'LABEL (+ 1 ctr)))
                               (define label3 (symnumapp 'LABEL (+ 2 ctr)))
                                (cons (list 'label label1)
                                      (append (main-parse (cadr exp))
                                              (cons (list 'branch (list -1 'SP) label2)
                                                    (cons (list 'sub 'SP 'SP 1)
                                                    (cons (list 'jump label3)
                                                          (cons (list 'label label2)
                                                                (cons (list 'sub 'SP 'SP 1)
                                                                (append (loop-parse (cddr exp) empty)
                                                                        (cons (list 'jump label1)
                                                                              (cons (list 'label label3) empty)))))))))))]
    ;for lone symbols and numbers
    [x (if (number? x)(cons (list 'move (list 0 'SP) x)(cons (list 'add 'SP 'SP 1) empty))
           (cons (list 'move (list 0 'SP) x)(cons (list 'add 'SP 'SP 1) empty)))]))
;main loop to loop through the main list
(define (main-func lst result)
  (cond
    [(empty? lst) result]
    [else (main-func (cdr lst) (append result (main-parse (car lst)) ))]))
;main function
(define (compile-simpl-helper lst)
  (begin (set! ctr -3)   ;reset main counter to -3
 (define val-list (setting-vars lst)) ;val-list is the list of vars
  (define almostdone 
  (if (false? val-list) (main-func lst empty) ;if false(no vars statement) just loop through the list
      (append (main-func (cddr lst) empty) (cons (list 'halt) empty)(set-datas val-list empty)))) ;if there is a vars statement, append it after the loop and halt 
  (define SPloc (length almostdone)) ; returns the length of the list rn
  (append almostdone (cons (list 'data 'SP '_End) (cons (list 'label '_End) empty))))); set SP to be the first unused memory
;if given an empty list just return empty

(define (compile-simpl lst)
  (if (empty? lst) empty (compile-simpl-helper lst)))


#|(define test2 (compile-simpl '(vars [(i 1) (j 0) (acc 0)]
  (while (<= i 10000)
     (set j 1)
     (set acc 0)
     (while (< j i)
        (iif (= (mod i j) 0)
             (set acc (+ acc j))
             (skip))
        (set j (+ j 1)))
     (iif (= acc i)
          (seq
            (print i)
            (print "\n"))
          (skip))
     (set i (+ i 1))))))
test2|#
#|(define test '(
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (move (0 (95)) 10000)
  (add (95) (95) 1)
  (le (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (branch (-1 (95)) 9)
  (jump 91)
  (sub (95) (95) 1)
  (move (0 (95)) 1)
  (add (95) (95) 1)
  (move (93) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (0 (95)) 0)
  (add (95) (95) 1)
  (move (92) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (0 (95)) (93))
  (add (95) (95) 1)
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (lt (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (branch (-1 (95)) 28)
  (jump 64)
  
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (move (0 (95)) (93))
  (add (95) (95) 1)
  (mod (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (0 (95)) 0)
  (add (95) (95) 1)
  (equal (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (branch (-1 (95)) 42)
  (jump 53)
  (move (0 (95)) (92))
  (add (95) (95) 1)
  (move (0 (95)) (93))
  (add (95) (95) 1)
  (add (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (92) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (jump 53)
  (move (0 (95)) (93))
  (add (95) (95) 1)
  (move (0 (95)) 1)
  (add (95) (95) 1)
  (add (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (93) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (jump 19)
  (move (0 (95)) (92))
  (add (95) (95) 1)
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (equal (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (branch (-1 (95)) 73)
  (jump 80)
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (print-val (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (print-string "\n")
  (jump 80)
  (move (0 (95)) (94))
  (add (95) (95) 1)
  (move (0 (95)) 1)
  (add (95) (95) 1)
  (add (-2 (95)) (-2 (95)) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (move (94) (-1 (95)))
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  (jump 0)
  (sub (95) (95) 1)
  (move (0 (95)) 0)
  0
  0
  0
  1
  96
))
|#
;test
;(define test-prog '((gt (3) 5 4) (move (0 (5)) #t) (branch (6) 4) (print-string "true")(print-string "true") 6))
;test-prog
;(load-primp test)
;(run-primp)