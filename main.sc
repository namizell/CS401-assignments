; Noah Mizell
; namizell
; 7 February 2018
; This program is meant to simplify s-expressions
; using algebraic simplification.

; Determines if x is a constant (c)
(define (constant? x) (number? x))

; Determines if the expression is a sum
(define (is-sum x)
  (and (pair? x) (equal? (car x) '+))
)

; Determines if the expression is a product
(define (is-product x)
  (and (pair? x) (equal? (car x) '*))
)

; Determines if the expression is a difference
(define (is-difference x)
  (and (pair? x) (equal? (car x) '-))
)

; Determines what kind of operation is used
(define (is-operation x)
  (or (is-sum x) (is-product x) (is-difference x))
)

; Returns if the x is equal to a '+'
(define (is-plus x) (equal? x '+))
; Returns if the x is equal to a '*'
(define (is-multiply x) (equal? x '*))
; Returns if the x is equal to a '-'
(define (is-subtract x) (equal? x '-))

; Return the first or last part of the operations
(define (addend e) (caddr e))
(define (augend e) (cadr e))

; Determines if two constants are being added
(define (add2constants? op p1 p2)
  (and (is-plus op) (constant? p1) (constant? p2))
)

; Determines if a constant is being addded to a term
(define (addconstant2term? op p1 p2)
  (and (is-plus op) (constant? p2))
)

; Determines if two constants are being multiplied
(define (multiply2constants? op p1 p2)
  (and (is-multiply op) (constant? p1) (constant? p2))
)

; Determines if a term is being multiplied by a constant
(define (multiplyconstant2term? op p1 p2)
  (and (is-multiply op) (constant? p2))
)

; Determines if a constant is being subtracted by a constant
(define (subtract2constants? op p1 p2)
  (and (is-subtract op) (constant? p1) (constant? p2))
)

; Determines if a term is being subtracted by a constant 
(define (subtractconstant2term? op p1 p2)
  (and (is-subtract op) (constant? p2))
)

; Set rules simplifying
; op is the operation
; p1 and p2 are the operands
(define (apply-rules op p1 p2)
  (cond
    ( (add2constants? op p1 p2)
      (+ p1 p2)
    )
    ( (addconstant2term? op p1 p2)
      (simplify (list '+ p2 p1))
    )
    ( (and (is-plus op) (is-sum p2))
      (simplify (list '+ (list '+ p1 (augend p2)) (addend p2)))
    )
    ( (multiply2constants? op p1 p2)
      (* p1 p2)
    )
    ( (multiplyconstant2term? op p1 p2)
      (simplify (list '* p2 p1))
    )
    ( (and (is-multiply op) (is-product p2))
      (simplify (list '* (list '* p1 (augend p2)) (addend p2)))
    )
    ( (subtract2constants? op p1 p2)
      (- p1 p2)
    )
    ( (subtractconstant2term? op p1 p2)
      (simplify (list '- p2 p1))
    )
    ( (and (is-subtract op) (is-difference p2))
      (simplify (list '- (list '- p1 (augend p2)) (addend p2)))
    )
    (else (list op p1 p2))
  )
)

; Simplifies the given expression
; exp is the expression
(define (simplify exp)
  (cond ((is-operation exp) (apply-rules (car exp) (simplify (cadr exp))                                              (simplify (caddr exp)))
        )
        (else exp)
  )
)

; A test for the program
; This expression can be changed
(simplify '(* 4 (* 5 (+ a 6)))

