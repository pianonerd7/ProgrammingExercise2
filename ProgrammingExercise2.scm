
; 1. dotproduct
(define dotproduct
  (lambda (l1 l2)
    (dotproduct-cps l1 l2 (lambda (v) v))))

(define dotproduct-cps
 (lambda (l1 l2 return)
   (if (or (null? l1) (null? l2))
       (return 0)
       (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (+ (* (car l1) (car l2)) v))))))

; 2. removesubsequence
(define removesubsequence
  (lambda (l1 l2)
    (removesubsequence-cps l1 l2 '())))

(define removesubsequence-cps
  (lambda (l1 l2 acc)
    (cond
      ((or(null? l1) (null? l2)) acc)
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) acc))
      (else (removesubsequence-cps l1 (cdr l2) (append acc (cons (car l2) '())))))))
