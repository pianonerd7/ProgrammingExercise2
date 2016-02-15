
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
  (lambda 

; 3. squareroot
(define squareroot
  (lambda (val iter)
    (squareroot-cps val iter (lambda (v) v))))

(define squareroot-cps
  (lambda (val iter return)
    (if (zero? iter)
        (return val)
        (squareroot-cps val (- iter 1) (lambda (v) (return (- v (/ (- (* v v) val) (* 2 v)))))))))

; 4. replacecall
(define replaceall
  (lambda (a1 a2 l return)
    (cond
      ((null? l) a1))))