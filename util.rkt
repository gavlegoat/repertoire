#lang racket

(provide switch)

;(define-syntax-parameter otherwise
;  (lambda (stx)
;    (raise-syntax-error (syntax-e stx) "can only be used inside switch")))

;(define otherwise '())

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
