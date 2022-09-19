#lang racket
;(require "Q8.rkt")
(define (sy sym1 sym2)
  (string->symbol (string-append (symbol->string sym1) (symbol->string sym2))))
(define ctr -3)
(define datalst empty)
(define hashtable (make-hash))
(define vatable (make-hash))
(define (loop-parse lst result)
  (cond
    [(empty? lst) result]
    [else (loop-parse (cdr lst) (append result (main-parse (car lst))))]))
(define (symnumapp sym1 num)
  (string->symbol (string-append "_"(symbol->string sym1) (number->string num))))
(define (list-help sym)
  (cons (list sym (list -2 'SP) (list -2 'SP) (list -1 'SP)) (cons (list 'sub 'SP 'SP 1) (cons (list 'move (list 0 'SP) 0) empty))))
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
    [`true             (cons (list 'move (list 0 'SP) #t) (cons (list 'add 'SP 'SP 1)empty))]
    [`false            (cons (list 'move (list 0 'SP) #f)(cons (list 'add 'SP 'SP 1)empty))]
    ;now stmts
    [`(set,var,aexp)   (append (main-parse aexp)(cons (list 'move (list (car (hash-ref vatable var)) 'FP) (list -1 'SP))(cons (list 'sub 'SP 'SP 1)(cons (list 'move (list 0 'SP) 0) empty))))]
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
    [`(return , aexp) (append (main-parse aexp) (cons (list 'move 'RETURN-VAL (list -1 'SP))(cons (list 'sub 'SP 'SP 1)(cons
  (list 'move (list 0 'SP) 0) empty))))]
    [(list fn arg ...) (begin (if (hash-has-key? hashtable fn) void (error "uninitalized" fn))
                              (set-fn-params (hash-ref hashtable fn) arg fn empty))]
    
    ;for lone symbols and numbers
    [x (if (number? x)(cons (list 'move (list 0 'SP) x)(cons (list 'add 'SP 'SP 1) empty))
           (cons (list 'move (list 0 'SP) (list (if (hash-has-key? vatable x)
                                                    (if (list?(hash-ref vatable x)) (car(hash-ref vatable x)) (hash-ref vatable x))
                                                    (error "uninitalized" x)) 'FP ))(cons (list 'add 'SP 'SP 1) empty)))]))
(define (set-fn-params fn-arg g-arg nm result)
  (cond
    [(empty? fn-arg) (if (empty? g-arg) (append result
                                                (list (list 'jsr 'RETURN-ADDR (sy 'FN_ nm)) (list 'move (list -1 'SP) 'RETURN-VAL)))
                         (sy 'START_ (error "arguments" g-arg fn-arg)))] ;not done yet, have to move to temp value
    [(empty? g-arg) (error "arguments" g-arg fn-arg)]
    [else (set-fn-params (cdr fn-arg) (cdr g-arg) nm (append result (main-parse (car g-arg))))]))

(define (main-func lst result)
  (cond
    [(empty? lst) result]
    [else (main-func (cdr lst) (append result (main-parse (car lst)) ))]))

(define (consmt lst sym op st result)
  (cond
    [(empty? lst) result]
    [else (begin
            (define x (if (list? (car lst)) (caar lst) (car lst)))
            (define syms (list 'const (sy sym x) st))
            (if (hash-has-key? vatable x) (error "duplicate") (if (list?(car lst)) (hash-set! vatable x (list (sy sym x)(cadar lst)))(hash-set! vatable x (sy sym x))))
                (consmt (cdr lst) sym op (op st 1) (cons syms result)))]))
 
(define (prolog sym lst)
  (define (prolog-help sym lst1 result)
  (cond
    [(empty? lst1) result]
    [else (prolog-help sym (cdr lst1)  (cons (list 'move (list (sy '_FN_ (sy sym (sy '_Var_ (caar lst1)))) 'SP) (cadr (hash-ref vatable (caar lst1)))) result))]))
  (append (list (list 'move (list (sy '_FN_ (sy sym '_FP_ )) 'SP) 'FP))
          (list (list 'move (list (sy '_FN_ (sy sym '_RETURN-ADDR)) 'SP) 'RETURN-ADDR))
          (prolog-help sym lst empty)
          (list (list 'move 'FP 'SP))
          (list (list 'add 'SP 'SP (sy '_FN_ (sy sym '_SIZE))))))
(define (eplog sym)
  (if (symbol=? sym 'main) (list (list 'halt))
  (list (list 'sub 'SP 'SP (sy '_FN_ (sy sym '_SIZE)))
        (list 'move 'FP (list (sy '_FN_ (sy sym '_FP_ )) 'SP))
        (list 'move 'RETURN-ADDR (list (sy '_FN_ (sy sym '_RETURN-ADDR)) 'SP))
        (list 'jump 'RETURN-ADDR))))

(define (funt lst)
  (begin (hash-clear! vatable) 
  (match lst
   [`(fun, nm,(list a b c ...)) (begin (check-return c empty)(define temp (consmt b (sy '_FN_ (sy (car nm) '_Var_)) + 2 empty))
    (append   (list(list 'label (sy 'FN_ (car nm))))
                                       (consmt (reverse(cdr nm)) (sy '_FN_ (sy (car nm) '_ARG_)) - -1 empty);;reverse becaues we have to evaluate args from left to right so leftmost is the lowest number
                                       (list(list 'const (sy '_FN_ (sy (car nm) '_FP_ )) 0 ))
                                       (list(list 'const (sy '_FN_ (sy (car nm) '_RETURN-ADDR )) 1))
                                        temp
                                       (list (list 'const (sy '_FN_ (sy (car nm)'_SIZE)) (+ 2(length  temp))))
                                       (prolog (car nm) b)
                                       (main-func c empty)
                                       (eplog (car nm))
                                       ))])))
 
(define (check-main lst)
  (cond
    [(empty? lst) #f]
    [else (if (symbol=? 'main (caadar lst)) #t (check-main (cdr lst)))]))

(define (check-return lst prev)
  (cond
    [(empty? lst) (if (empty? prev) (error "no return in" lst)(if (symbol=? 'return (car prev)) (void 3) (error "no return, last line is" prev)))]
    [else (check-return (cdr lst) (car lst))]))
(define (set-func lst)
  (cond
    [(empty? lst) (void)]
    [else (begin (if (hash-has-key? hashtable (caadar lst)) (error "duplicate") (hash-set! hashtable (caadar lst) (cdadar lst))) (set-func (cdr lst)))]))

(define (compile-simpl lst)
  (begin (hash-clear! hashtable)
  (define x (check-main lst))
  (set-func lst)
  (define(funt-loop lst1 result)
    (cond
      [(empty? lst1) result]
      [else (funt-loop (cdr lst1) (append result (funt (car lst1))))]))
  (if x (append (list (list 'jump 'FN_main)) (funt-loop lst empty) end) (append (funt-loop lst empty) end))))

(define end '((data RETURN-VAL 0)
(data RETURN-ADDR 0)
(data FP 0)
(data SP END)
(halt)
(label END)))
(define test1 '((fun (countdown n)
  (vars [(result 0)]
    (iif (> n 0)
         (set result (countdown (- n 1)))
         (skip))
    (return result)))
(fun (main) 
  (vars [(n 9900)] 
    (return (countdown n))))))
#|(funt '(fun (main)
  (vars [(result 0)(esh 4) (sjdhf 7)]
    (print result)
    (print "\n")
    (iif (> result 0)
         (set result 5)
         (skip))
    (return (print result)))))|#
;vatable
;test1
(compile-simpl test1)