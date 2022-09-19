#lang racket
(require "primpl.rkt")
(provide primpl-assemble)
(struct Vari (loc val type) #:transparent #:mutable)
(define hashtable (make-hash))

(define (parse sy ctr)
 (match sy
            [`(data, x ,val)   (if (hash-has-key? hashtable (parse x ctr)) (error "duplicate"(parse x ctr))
                                   (if (list? val)
                                       (hash-set! hashtable (parse x ctr) (Vari ctr (cadr val) 'data))
                                       (hash-set! hashtable (parse x ctr) (Vari ctr val 'data))))]
            [`(const, x , val) (if (hash-has-key? hashtable (parse x ctr))
                                   (error "duplicate"(parse x ctr))
                                   (hash-set! hashtable (parse x ctr ) (Vari (void 3) val 'const)))]
            [`(label, name)    (if (hash-has-key? hashtable (parse name ctr))
                                   (error "duplicate" (parse name ctr))
                                   (hash-set! hashtable (parse name ctr) (Vari (void 3) ctr 'label )))]
            [x x]
            [else sy]
            ))

(define (helplookup sym prev)
  (cond
    [(not(hash-has-key? hashtable sym)) (error "undefined" sym)]
    [else                                      
  (define temp (hash-ref hashtable sym))
  (cond
    [(equal? sym prev) (error "circular")]
    [(symbol? (Vari-val temp)) (if (empty? prev) (helplookup (Vari-val temp) sym)(helplookup (Vari-val temp) prev))]
    [(void? (Vari-val temp)) (Vari-loc temp)]
    [(void? (Vari-loc temp))  (Vari-val temp)]
    [(number? (Vari-val temp))(cons (Vari-loc temp) empty)  ];;;;;(cons (Vari-loc temp) empty)
    )]))

(define (lookup sym)
  (match sym
    [(list imm ind)
     (cond
       [(symbol? imm) (if (symbol? ind) (cons (helplookup imm empty) (cons (helplookup ind empty) empty))(cons (car (lookup imm)) (cons (lookup ind) empty)))]
       [else (if (symbol? ind) (cons imm (list (helplookup ind empty)))(cons imm (cons (lookup ind) empty)))])]
    [(list nat) (list nat)]
    [x (if (symbol? x) (helplookup x empty) x)]))

(define (datacont times num result)
  (cond
    [(= 0 times) result]
    [else (datacont (- times 1) num (cons num result))]))

(define (databack lst result)
  (cond
    [(empty? lst) result]
    [else (databack (cdr lst)(cons(car lst) result))]))

(define (addvars lst result ctr)
  (cond
    [(empty? lst) result]
    [(symbol=? (caar lst) 'const) (begin (parse (car lst) ctr) (addvars (cdr lst ) (cons (car lst) result) ctr))]
    [(symbol=? (caar lst) 'data)  (begin (parse (cons (caar lst) (cons (cadar lst) (cons (caddar lst) empty))) ctr)
                                     (if (list?(caddar lst))
                                         (if (symbol? (car(caddar lst)))
                                             (addvars (cdr lst) (append  (cdddar lst) (cons (cons 'data (cons (cadar lst) (cons (caddar lst) empty))) result)) (+ ctr (length (cddar lst)))) 
                                     (addvars (cdr lst) (append (datacont (- (lookup (car(caddar lst))) 1 ) (cadr(caddar lst)) empty)
                                                                (cons (cons 'data (cons (cadar lst) (cons (cadr (caddar lst))empty))) result))
                                              (+ ctr (car(caddar lst)))))
                                     (addvars (cdr lst) (append  (cdddar lst) (cons (cons 'data (cons (cadar lst) (cons (caddar lst) empty))) result)) (+ ctr (length (cddar lst))))))]
    [(symbol=? (car(car lst)) 'label) (begin (parse (car lst) ctr) (addvars (cdr lst) result ctr))]
    [else (begin (parse (car lst) ctr) (addvars (cdr lst)(cons (car lst) result) (+ 1 ctr)))]))

(define (main-parse sy)
  (match sy
    [`(add ,dest ,opd1 ,opd2) (cons 'add (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(sub, dest, opd1, opd2) (cons 'sub (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(mul, dest, opd1, opd2) (cons 'mul (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(div, dest, opd1, opd2) (cons 'div (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(mod, dest, opd1, opd2) (cons 'mod (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(gt, dest, opd1, opd2) (cons 'gt (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(lt, dest, opd1, opd2) (cons 'lt (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(le, dest, opd1, opd2) (cons 'le (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(equal, dest, opd1, opd2) (cons 'equal (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(land, dest, opd1, opd2) (cons 'land (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(lor, dest, opd1, opd2) (cons 'lor (cons (main-parse dest) (cons (main-parse opd1)(cons (main-parse opd2) empty))))]
    [`(lnot, dest, opd) (cons 'lnot (cons (main-parse dest) (cons (main-parse opd) empty)))]
    [`(jump, opd) (cons 'jump (cons (main-parse opd) empty))]
    [`(branch, opd1, opd2) (cons 'branch (cons (main-parse opd1) (cons (main-parse opd2)empty)))]
    [`(move, dest,opd) (cons 'move (cons (main-parse dest) (cons (main-parse opd) empty)))]
    [`(print-val, opd) (cons 'print-val (cons (main-parse opd)empty))]
    [`(print-string, string) (cons 'print-string (cons string empty))]
    [`(const,var,val) (begin (hash-set! hashtable var (Vari (void 3) (main-parse val) const)) empty)]
    [`(halt) 0]
    [`(lit ,val) (main-parse val)]
    [`(jsr, x,y) (list 'jsr (main-parse x) (main-parse y))]
    [`(data, sym, val) (if (list? (main-parse val) ) (car (main-parse val)) (main-parse val))]
    [ (list a b) (list (main-parse a) (main-parse b))]
    [x (lookup x)]))

(define (main-loop lst result)
  (cond
    [(empty? lst) result]
    [(empty? (main-parse (car lst))) (main-loop (cdr lst) result)]
    [else (main-loop (cdr lst) (cons (main-parse (car lst)) result))]))

;(define test '((label nane)(+ s f)(data z 5)(data y z)))

(define (primpl-assemble lst)
  (begin (hash-clear! hashtable)
  (reverse (main-loop (main-loop (addvars lst empty 0) empty) empty))))




(define test1 '((jump 39)
  (move (0 (55)) (54)) ;57
  (move (1 (55)) (53)) ;58
  (move (2 (55)) 0) ;59
  (move (54) (55))
  (add (55) (55) 3);SP = 60
  (move (0 (55)) (-1 (54)));60
  (add (55) (55) 1);Sp = 61
  (move (0 (55)) 0);61
  (add (55) (55) 1);SP = 62
  (gt (-2 (55)) (-2 (55)) (-1 (55)))
  (sub (55) (55) 1)
  (move (0 (55)) 0);SP = 61
  (branch (-1 (55)) 16)
  (sub (55) (55) 1);SP = 60
  (jump 30)
  (sub (55) (55) 1);SP = 60
  (move (0 (55)) (-1 (54)))
  (add (55) (55) 1);Sp = 61
  (move (0 (55)) 1)
  (add (55) (55) 1);Sp = 62
  (sub (-2 (55)) (-2 (55)) (-1 (55)))
  (sub (55) (55) 1)
  (move (0 (55)) 0);Sp = 61
  (jsr (53) 1)
  (move (-1 (55)) (52))
  (move (2 (54)) (-1 (55)))
  (sub (55) (55) 1);SP = 60
  (move (0 (55)) 0)
  (jump 30)
  (move (0 (55)) (2 (54)))
  (add (55) (55) 1);SP = 61
  (move (52) (-1 (55)))
  (sub (55) (55) 1)
  (move (0 (55)) 0);SP = 60
  (sub (55) (55) 3);SP = 57
  (move (54) (0 (55)))
  (move (53) (1 (55)))
  (jump (53))
  (move (0 (55)) (54))
  (move (1 (55)) (53))
  (move (2 (55)) 1000)
  (move (54) (55))
  (add (55) (55) 3);SP = 57
  (move (0 (55)) (2 (54)))
  (add (55) (55) 1);SP = 58
  (jsr (53) 1)
  (move (-1 (55)) (52))
  (move (52) (-1 (55)))
  (sub (55) (55) 1);SP = 57
  (move (0 (55)) 0)
  0
  0
  0
  0
  57
  0))
test1
;(define test2 '((label a) (data b 2) (jump a) (branch b a) (jump b) (branch b b)(print-val b)))
;(define test3 '((label a) (const A 2) (const B A) (jump B) (branch B A) (print-val B)))
;(hash-clear! hashtable)
;(lookup '(lit 3))
;(primpl-assemble test1)
;(define test-prog (primpl-assemble test1))
;test-prog

;(addvars test1 empty 0)
;(hash-clear hashtable)
;hashtable

(load-primp test1)
(run-primp)
;Problems:
;check for undefined
;need to split this into three main loops
;const check if works