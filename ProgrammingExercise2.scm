; Anna He

; 1. dotproduct
;Takes two lists of numbers and computes the dot product. If one list is longer, the extra numbers are ignored.
(define dotproduct
  (lambda (l1 l2)
    (dotproduct-cps l1 l2 (lambda (v) v))))

(define dotproduct-cps
  (lambda (l1 l2 return)
    (if (or (null? l1) (null? l2))
        (return 0)
        (dotproduct-cps (cdr l1) (cdr l2) (lambda (v) (return (+ (* (car l1) (car l2)) v)))))))

; 2. removesubsequence
;Takes 2 lists of atoms. The first list is a subsequence of the second list, and removesubsequence
;return the second list with the frist occurence of the subsequence removed. 
(define removesubsequence
  (lambda (l1 l2)
    (removesubsequence-cps l1 l2 (lambda (v) v))))

(define removesubsequence-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return l2))
      ((eq? (car l1) (car l2)) (removesubsequence-cps (cdr l1) (cdr l2) (lambda (v) (return v))))
      (else (removesubsequence-cps l1 (cdr l2) (lambda (v) (return (cons (car l2) v))))))))

; 3. squareroot
;Takes a value and an interation. squareroot will compute the square root of the value using iteration
;rounds of Newton's method starting with an initial value to the input value.
(define squareroot
  (lambda (val iter)
    (squareroot-cps val iter (lambda (v) v))))

(define squareroot-cps
  (lambda (val iter return)
    (if (zero? iter)
        (return val)
        (squareroot-cps val (- iter 1) (lambda (v) (return (- v (/ (- (* v v) val) (* 2 v)))))))))

; 4. replacecall
;Takes two atoms and a nested list and replaces every occurrence of the first atom with the second.
(define replaceall*
  (lambda (a1 a2 l)
    (replaceall*-cps a1 a2 l (lambda (v) v))))

(define replaceall*-cps
  (lambda (a1 a2 l return)
    (cond
      ((null? l) (return l))
      ((list? (car l)) (replaceall*-cps a1 a2 (car l)
                                        (lambda (v1) (replaceall*-cps a1 a2 (cdr l)
                                                                      (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car l) a1) (replaceall*-cps a1 a2 (cdr l) (lambda (v) (return (cons a2 v)))))
      (else (replaceall*-cps a1 a2 (cdr l) (lambda (v) (return (cons (car l) v))))))))

; 5. reverse*
;Takes a nested list and reverses the contents of the list and all nested lists.
(define reverse*
  (lambda (l)
    (reverse*-cps l (lambda (v) v))))

(define reverse*-cps
  (lambda (l return)
    (cond
      ((null? l) (return l))
      ((pair? (car l)) (reverse*-cps (car l)
                                     (lambda (v1) (reverse*-cps (cdr l)
                                                                (lambda (v2) (return (append v2 (cons v1 '()))))))))
      (else (reverse*-cps (cdr l) (lambda (v) (return (append v (cons (car l) '())))))))))

; 6. vectormult
;Takes a row vector (list of numbers) and a matrix (a list of lists of numbers) and multiplies
;the vector times the matrix.
(define firstcolumn-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (firstcolumn-cps (cdr m) (lambda (v) (return (cons (caar m) v)))))))

(define restOfColumns-cps
  (lambda (m return)
    (if (null? m)
        (return '())
        (restOfColumns-cps(cdr m) (lambda (v) (return (cons (cdar m) v)))))))

(define vectormult
  (lambda (rowvector matrix)
    (vectormult-cps rowvector matrix (lambda (v) v))))

(define vectormult-cps
  (lambda (rowvector matrix return)
    (if (or (null? matrix) (null? (car matrix)))
        (return '())
        (vectormult-cps rowvector (restOfColumns-cps matrix (lambda (v) v)) (lambda (v) (return (cons (dotproduct rowvector (firstcolumn-cps matrix (lambda (v) v))) v)))))))

; 7. matrixmultiply
;Takes two matrices (a list of lists of numbers) and multiplies them.
(define matrixmultiply
  (lambda (matrix1 matrix2)
    (matrixmultiply-cps matrix1 matrix2 (lambda (v) v))))

(define matrixmultiply-cps
  (lambda (matrix1 matrix2 return)
    (if (null? matrix1)
        (return '())
        (matrixmultiply-cps (cdr matrix1) matrix2 (lambda (v) (return (cons (vectormult (car matrix1) matrix2) v)))))))

; 8. removesubsequence*
;Takes a list of atoms and a general list. The first list is a subsequence of the second list.
;removesubsequence* return the second list with the first occurence of the subsequence removed.
(define removesubsequence*
  (lambda (atoms list)
    (removesubsequence*-cps atoms list (lambda (v1 v2) v2))))

(define removesubsequence*-cps
  (lambda (atoms list return)
    (cond
      ((or (null? atoms) (null? list)) (return atoms list))
      ((and (list? (car list))(null? (cdr list))) (removesubsequence*-cps atoms (car list) (lambda (v1 v2) (return v1 (cons v2 '()))))) 
      ((list? (car list)) (removesubsequence*-cps atoms (car list)
                                                  (lambda (v1 v2) (removesubsequence*-cps v1 (cdr list)
                                                                                          (lambda (v3 v4) (return v3 (cons v2 v4)))))))
      ((eq? (car atoms) (car list)) (removesubsequence*-cps (cdr atoms) (cdr list) (lambda (v1 v2) (return (cdr atoms) v2)))) 
      (else (removesubsequence*-cps atoms (cdr list) (lambda (v1 v2) (return v1 (cons (car list) v2))))))))

; 9. suffix with letrec
;Takes an atom and a list and returns a list containing all elements that occur after the last
;occurence of the atom.
(define suffix
  (lambda (atom list)
    (letrec
        ((search
          (lambda (atom list return)
            (cond
              ((null? list) (return list))
              ((eq? (car list) atom) (search atom (cdr list) (lambda (v) v)))
              (else (search atom (cdr list) (lambda (v) (return (cons (car list) v)))))))))
      (search atom list (lambda (v) v)))))

; 10. suffix with call/cc
;Does the same thing as suffix but computes using call/cc.
(define suffix2
  (lambda (atom list)
    (call/cc
     (lambda (break)
       (letrec
           ((search
             (lambda (atom list return)
               (cond
                 ((null? list) (return list))
                 ((eq? (car list) atom) (search atom (cdr list) (lambda (v) (break v))))
                 (else (search atom (cdr list) (lambda (v) (return (cons (car list) v)))))))))
         (search atom list (lambda (v) v)))))))