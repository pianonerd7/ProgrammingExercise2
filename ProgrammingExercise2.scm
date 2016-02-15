
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
(define removesubsequence-cps
  (lambda (l1 l2 return)
    (cond
      ((or(null? l1) (null? l2)) (return '()))
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) (lambda (v) v)))
      (else (removesubsequence-cps l1 (cdr l2) (lambda (v) (cons (car l2) v)))))))

(define emovesubsequence-cps
  (lambda (l1 l2 acc)
    (cond
      ((or(null? l1) (null? l2)) acc)
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) acc))
      (else (removesubsequence-cps l1 (cdr l2) (lambda (v) (cons (car l2) v)))))))

(define removesubsequence
  (lambda (l1 l2)
    (cond
      ((or (null? l1) (null? l2)) l2)
      ((eq? (car l1) (car l2)) (removesubsequence (cdr l1) (cdr l2)))
      (else (cons (car l2) (removesubsequence l1 (cdr l2)))))))

(define sumnumbers-acc
  (lambda (l acc)
    (cond
      ((null? l) acc)
      ((number? (car l)) (sumnumbers-acc (cdr l) (+ (car l) acc)))
      (else (sumnumbers-acc (cdr l) acc)))))