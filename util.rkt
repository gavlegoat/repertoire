#lang racket

(provide switch)

;(define-syntax-parameter otherwise
;  (lambda (stx)
;    (raise-syntax-error (syntax-e stx) "can only be used inside switch")))

;(define otherwise '())

;; Switch effectively works like a restricted cond statement:
;; (switch a [b1 c1] [b2 c2] ... [bn cn]) is equivalent to
;; (cond [(equal? a b1) c1]
;;       [(equal? a b2) c2]
;;       ...
;;       [(equal? a bn) cn])
;; Note that switch can also take an else clause.
(define-syntax-rule (switch v clauses ...)
  (let ([tmp v])
    (switch/clauses tmp clauses ...)))

(define-syntax switch/clauses
  (syntax-rules (else)
    [(_ val) (void)]
    [(_ val (else v1 v2 ...)) (begin v1 v2 ...)]
    [(_ val (test branch1 branch2 ...) rest ...)
     (if (equal? val test)
         (begin branch1 branch2 ...)
         (switch/clauses val rest ...))]))
