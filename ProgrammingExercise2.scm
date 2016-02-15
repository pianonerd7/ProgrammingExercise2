
; 1. dotproduct
(define dotproduct-cps
 (lambda (l1 l2 return)
   (if (or (null? l1) (null? l2))
       (return 0)
       (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (+ (* (car l1) (car l2)) v))))))

(define dotproduct
  (lambda (l1 l2)
    (dotproduct-cps l1 l2 (lambda (v) v))))