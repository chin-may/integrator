;; Symbolic Integration in LISP
;; Written as a part of the course CS310 - Paradigms of Programming
;; Authors : Akhilesh Godi (CS10B037) and Chinmay Bapat (CS10B059)

 
(defun integrate (expr dvar)
  (cond
    ((notContainsVariable expr dvar ) `(* ,expr ,dvar))
    ((numberp expr) (make-prod expr dvar))
;;    ((symbolp expr) (make-div (make-pow expr 2) 2))
;;    ((isPow expr dvar) 
;;     (make-div (make-pow dvar (make-sum (exponent expr) 1)) (make-sum (exponent expr) 1))
;;     )
    ((isAdd expr) (make-sum (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
    ((isUnaryMinus expr) `(- , (integrate (cadr expr) dvar) ))
    ((isSub expr)  (make-sub (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
    
    ;;  Split the expression up into 2 parts : One that contains x and one that does not.
    ;;  Call the one that contains x as factorsContainingX and the one that does not as const-factors
    
    ((multiple-value-bind (const-factors factorsContainingX)
       (partition-if #'(lambda (factor) (notContainsVariable factor dvar))
                       (factorize expr))
       ;; Now integrate and return
       `(* 
             ,(makeProductOfFactors const-factors)                   
             
             ,(cond ((null factorsContainingX) dvar)                     ;; If there are no factors containing variable return var
                    
                    ;; Try to integrate and check if the derivative divides the integral or other such forms
                    ((some #'(lambda (factor) (divideWithDerivative factor factorsContainingX dvar)) factorsContainingX))                          
                           
                    ;; u-v rule for integration!
                    ((isProd expr) 
                         (cond 
                           ((numberp (cadr expr)) (make-prod (cadr expr) (integrate (caddr expr) dvar)))
                           ((numberp (caddr expr)) (make-prod (caddr expr) (integrate (cadr expr) dvar)))
                           (t  (let ((iuv (make-sub
                                            (make-prod (caddr expr) (integrate (cadr expr) dvar))
                                            (integrate 
                                              (make-prod 
                                                (integrate (cadr expr) dvar) (differentiate (caddr expr) dvar)) dvar)
                                            )

                                          )
                                     )
                                 (if (or (find 'integrate iuv) (findnil iuv))
                                   (make-sub
                                     (make-prod (cadr expr) (integrate (caddr expr) dvar))
                                     (integrate 
                                       (make-prod 
                                         (differentiate (cadr expr) dvar) (integrate (caddr expr) dvar)) dvar)
                                     )
                                   iuv
                                   )
                                 )

                               )
                         ))
                   
                    (t `(INTEGRATE ,(makeProductOfFactors factorsContainingX) ,dvar))))))
    )
  )
 
;; All the rules of integration go here! 
(defun integrateFromTable (expr dvar)
  (cond 
    ;;((and (isProd expr) (numberp (cadr expr))) (make-prod (cadr expr) (integrate (caddr expr) dvar)))
    ;;((and (isProd expr) (numberp (caddr expr))) (make-prod (caddr expr) (integrate (cadr expr) dvar)))
    ;;((and (isDiv expr) (numberp (divisor expr))) (make-div (integrate (dividend expr) dvar) (divisor expr)))
    ((isSin expr)
     (cond 
       ((eq dvar (cadr expr)) 
        `(- (cos ,dvar))
        )
       ((isAx (cadr expr) dvar) 
        `(- ,(make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar))) 
        )
       ((isXb (cadr expr) dvar)
        `(- (cos ,(cadr expr)))
        )
       ((isAxb (cadr expr) dvar)
        `(- ,(make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar)))
        )
       ((or (isXbyA (caddr expr) dvar) (isXbyAplusb (caddr expr) dvar))
        `(- ,(make-prod (list 'cos (cadr expr)) (getA (cadr expr) dvar)))
        )
        (t `(- (cos ,(cadr expr))))
       )
     )
    
    ((isLog expr) 
     `(- (* ,dvar (log ,(cadr expr))) ,dvar)
     )
    ((isTan expr) 
     `(- (log (cos ,(cadr expr))))
     )
    ((isCos expr)
     (cond 
       ((eq dvar (cadr expr)) 
        `(sin ,dvar)
        )
       ((isAx (cadr expr) dvar) 
        (make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar))
        )
       ((isXb (cadr expr) dvar)
        `(sin ,(cadr expr))
        )
       ((isAxb (cadr expr) dvar)
         (make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar))
        )
       ((or (isXbyA (caddr expr) dvar) (isXbyAplusb (caddr expr) dvar))
          (make-prod (list 'cos (cadr expr)) (getA (cadr expr) dvar))
        )
        (t `(sin ,(cadr expr)))
       )
     )
    
    ((isExp expr) 
     (handleExp expr dvar))
    ((isAExp expr dvar) (handleAExp expr dvar))
    ((and (isDiv expr) (isAtanForm expr dvar))
     (cond 
       ((numberp (divisor expr))
        (make-div (integrate (dividend expr) dvar) (divisor expr)))
       ((isAtanForm expr dvar)
        `( (/ (atan (/ ,dvar (sqrt ,(getAtanA expr dvar)))) (sqrt ,(getAtanA expr dvar))))
        )
       )
     )
     ))
     
     
(defun isSpecial (expr dvar)
    (cond   
            ((atom expr) nil)
            ((eq (length expr) 3) nil)
            ((eq (length expr) 2) t)
            ((or (isExp expr) (isAExp expr dvar)) t)
            (t nil)))
            
(defun getAtanA (expr dvar)
  (if (notContainsVariable (cadr (caddr expr)) dvar) (cadr (caddr expr))
    (caddr (caddr expr))
    )
  )

(defun isAtanForm (expr dvar)
  (if (atom expr) nil
  (if (listp (caddr expr)) 
  (and (eq '/ (car expr)) 
       (notContainsVariable (cadr expr) dvar)
       (or
         (and (notContainsVariable (cadr (caddr expr)) dvar) (isSqr (caddr (caddr expr)) dvar))
         (and (notContainsVariable (caddr (caddr expr)) dvar) (isSqr (cadr (caddr expr)) dvar))
         )
       (eq '+ (car (caddr expr)))
       )))
  )
  
(defun isSqr (expr dvar)
  (if (atom expr) nil
  (and (eq '^ (car expr))
       (eq dvar (cadr expr))
       (eq 2 (caddr expr))
       ))
  )


(defun handleExp (expr dvar)
  (cond
    ((eq dvar (cadr expr) )
     `(expo ,dvar)
     )
    ((isAx (cadr expr) dvar)
     (make-div `(expo ,(cadr expr))  (getA (cadr expr) dvar))
     )
    ((isXb (cadr expr) dvar)
     `(expo , (caddr expr))
     )
    ((isAxb (cadr expr) dvar)
     (make-div `(expo ,(cadr expr))  (getA (cadr expr) dvar))
     )
    ((or (isXbyA (cadr expr) dvar) (isXbyAplusb (cadr expr) dvar))
     (make-prod `(expo ,(cadr expr))  (getA (cadr expr) dvar))
     )
    (T `(expo ,expr))
    )
  )

(defun handleAExp (expr dvar)
  (cond
    ((eq dvar (caddr expr) )
     `(/ (_ ,(cadr expr) ,dvar) (log ,(cadr expr)))
     )
    ((isAx (caddr expr) dvar)
     `(/ ,(make-div `(_ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr)))
     )
    ((isXb (caddr expr) dvar)
     `(/ (_ ,(cadr expr) ,(caddr expr)) (log ,(cadr expr)))
     )
    ((isAxb (caddr expr) dvar)
     `(/ ,(make-div `(_ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr)))
     )
    ((or (isXbyA (caddr expr) dvar) (isXbyAplusb (caddr expr) dvar))
     `(/ ,(make-prod `(_ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr)))
     )
    (T `(* (_ ,(cadr expr) ,(caddr expr)) (log ,(cadr expr))))
    )
  )
  
(defun isAx (expr dvar)
  (if (atom expr)
    nil
    (and (eq '* (car expr)) 
         (or 
           (and (eq dvar (cadr expr)) (notContainsVariable (caddr expr) dvar ))
           (and (eq dvar (caddr expr)) (notContainsVariable (cadr expr) dvar ))
           )
         )
    )
  )


(defun isXb (expr dvar)
(if (atom expr) nil
  
  (and (or (eq '+ (car expr)) (eq '- (car expr)))
       (or 
         (and (eq dvar (cadr expr)) (notContainsVariable (caddr expr) dvar))
         (and (eq dvar (caddr expr)) (notContainsVariable (cadr expr) dvar))
         )
       )) 
  )

(defun isXbyA (expr dvar)
  (if (atom expr) nil
    (and (eq '/ (car expr)) 
         (or 
           (and (eq dvar (cadr expr)) (notContainsVariable (caddr expr) dvar ))
           (and (eq dvar (caddr expr)) (notContainsVariable (cadr expr) dvar ))
           )
    ))
    )
    

(defun isAxb (expr dvar)
    (if (atom expr) nil
  
  (  and (or (eq '+ (car expr)) (eq '- (car expr)))
         (or (and (isAx (cadr expr) dvar) (notContainsVariable (caddr expr) dvar ))
             (and (isAx (caddr expr) dvar) (notContainsVariable (cadr expr) dvar ))
             )
         ))
  )

(defun isXbyAplusb (expr dvar)
(if (atom expr) nil
  
  (and (or (eq '+ (car expr)) (eq '- (car expr)))
       (or (and (isXbyA (cadr expr) dvar) (notContainsVariable (caddr expr) dvar ))
           (and (isXbyA (caddr expr) dvar) (notContainsVariable (cadr expr) dvar ))
           )
       ))
  )


(defun getA (expr dvar)
  (if (isAx expr dvar)
    (if (notContainsVariable (cadr expr) dvar ) 
      (cadr expr)
      (caddr expr)
      )
    (if (isAx (cadr expr) dvar) 
      (getA (cadr expr) dvar)
      (getA (caddr expr) dvar)
      )
    )
  )

(defun getB (expr dvar)
  (if (isAx (cadr expr))
    (caddr expr)
    (cadr expr)
    )
  )

(defun isTan (expr)
(if (atom expr) nil
  
  (eq (car expr) 'tan))
  )

(defun isExp (expr)
  (if (atom expr) nil
     (eq (car expr) 'expo)
    )
  )

(defun isAExp (expr dvar)
  (if (atom expr) nil
  
  (and (eq (car expr) '_) (notContainsVariable (cadr expr) dvar))))

(defun isLog (expr)
  (if (atom expr) nil
    (eq (car expr) 'log))
  )

(defun isSin (expr)
  (if (atom expr) nil
    (eq (car expr) 'sin))
  )

(defun isCos (expr)
 (if (atom expr) nil
  (eq (car expr) 'cos))
  )

(defun isDiv (expr)
  (if (atom expr) nil
    (eq '/ (car expr)))
  )

(defun isPow (expr dvar)
  (if (atom expr) nil
    (and (eq (car expr) '^) (eq (cadr expr) dvar) (notContainsVariable (caddr expr) dvar)))
  )

(defun isAdd (expr)
    (if (atom expr) nil
      (eq '+ (car expr))))

(defun isProd (expr)
    (if (atom expr) nil
      (eq '* (car expr))))

(defun isUnaryMinus (expr)
    (if (atom expr) nil
    (and (eq '- (car expr)) (eq (length expr) 2)))
  )

(defun isSub (expr)
  (if (atom expr) nil
    (and (eq '- (car expr)) (eq (length expr) 3))
    )
  )


(defun differentiate (expr dvar)
  (cond 
    ((notContainsVariable expr dvar) 0)
    ((symbolp expr) 1)
    ((isAdd expr) (make-sum (differentiate (cadr expr) dvar) (differentiate (caddr expr) dvar)))
    ((isSub expr) (make-sub (differentiate (cadr expr) dvar) (differentiate (caddr expr) dvar)))
    ((isProd expr) 
     (make-sum 
       (make-prod (multiplier expr) (differentiate (multiplicand expr) dvar))
       (make-prod (multiplicand expr) (differentiate (multiplier expr) dvar))
       )
     )
    ((isDiv expr)
     (if (notContainsVariable (divisor expr) dvar)
       (make-div (differentiate (dividend expr) dvar) (divisor expr))
       (make-div 
         (make-sub 
           (make-prod (divisor expr) (differentiate (dividend expr) dvar))
           (make-prod (dividend expr) (differentiate (divisor expr) dvar))
           )
         (list '^ (divisor expr) 2)
         )
       )
     )
    ((isPow expr dvar) (make-prod (make-pow (base expr) (make-sub (exponent expr) 1))(exponent expr)))
    ((isSin expr) (make-prod (list 'cos (cadr expr)) (differentiate (cadr expr) dvar)))
    ((isCos expr) (make-prod (list '- (list 'sin (cadr expr))) (differentiate (cadr expr) dvar)))
    ((isExp expr) (make-prod expr (differentiate (caddr expr) dvar)))
    ((isAExp expr dvar) (make-prod (list 'log (cadr expr)) (make-prod expr (differentiate (caddr expr) dvar)) ))
    ((isLog expr) (make-div (differentiate (cadr expr) dvar) (cadr expr)))
    ((isTan expr) (make-prod `(/ 1 (^ (cos ,(cadr expr)) 2))  (differentiate (cadr expr) dvar)))
    )
  )

(defun multiplier (expr)
  (cadr expr)
  )

(defun multiplicand (expr)
  (caddr expr)
  )

(defun base (expr)
  (cadr expr)
  )

(defun exponent (expr)
  (caddr expr)
  )

(defun divisor (expr)
  (caddr expr)
  )

(defun dividend (expr)
  (cadr expr)
  )

(defun make-sum (l r)
  (cond 
    ((and (numberp l) (numberp r))
     (+ l r))
    ((eq 0 l) r) 
    ((eq 0 r) l) 
    (T (list '+ l r)) 
    ) 
  )

(defun make-sub (l r)
  (cond 
    ((and (numberp l) (numberp r))
     (- l r))
    ((eq 0 l) (list '- r)) 
    ((eq 0 r) l) 
    (T (list '- l r)) 
    ) 
  )

(defun make-prod (l r)
  (cond ((or (eq 0 l) (eq 0 r)) 0)
        ((eq 1 l) r)
        ((eq 1 r) l)
        ((and (numberp l) (numberp r)) (* l r))
        (T (list '* l r))
        )
  )

(defun make-pow (base exp)
  (cond 
    ((eq 0 exp) 1)
    ((eq 1 exp) base)
    (T (list '^ base exp))
    )
  )

(defun make-div (num den)
  (cond 
    ((eq 0 num) 0)
    ((eq 1 den) num)
    ((eq 0 den) nil)
    (T (list '/ num den))
    )
  )

(defun beginsWith ( lst x )
    (and (consp lst) (eql (first lst) x )))

;; Convert a list of factors back into prefix form.
(defun makeProductOfFactors (factors)
  (cond ((null factors) 1)
        ((and (consp factors) (null (cdr factors))) (car factors))
        (t `(* ,(first factors) ,(makeProductOfFactors (cdr factors))))))

(defun notContainsVariable (expr var)
  (not (find-Variable var expr))
  )


(defun find-Variable (var expr)
  (cond ((eql var expr) expr)
        ((atom expr) nil)
        ((find-Variable var (car expr)))
        ((find-Variable var (cdr expr)))
        )
  )

(defun findnil (expr)
  (cond
    ((atom expr) (null expr))
    ((null (cdr expr)) (findnil (car expr)))
    (T (or (findnil (car expr)) (findnil (cdr expr))))
    )
  )


(defun inprint (expr)
  (cond ((atom expr) expr)
    ((eql (length expr) 2) expr)
    (t (list (inprint (cadr expr)) (inprint (car expr)) (inprint (caddr expr))))
    )
  )

(defun divideWithDerivative (factor factors x)
  (let* ((fx (cadr factor))             
         (n (caddr factor))
         (fact1 (divide-factors factors (factorize `(* ,factor ,(differentiate fx x))))))
    (cond ((notContainsVariable fact1 x)
           (if (= n -1)
               `(* ,(makeProductOfFactors fact1) (log ,fx))
               `(/ (* ,(makeProductOfFactors fact1) (^ ,fx ,(+ n 1))) ,(+ n 1))))
          ((and (= n 1) (or (isSin fx) (isCos fx) (isTan fx) (isLog fx) (isexp fx) (isAexp fx x) (isAtanform fx x)) )
           (let ((fact2 (divide-factors factors (factorize `(* ,fx ,(differentiate (cadr fx) x))))))
             (if (notContainsVariable fact2 x)
                 `(* ,(integrateFromTable fx x) ,(makeProductOfFactors fact2))))))))
 


;; TODO : Need to simplify expressions
(defun simplify (expr)
  (cond 
    ((isAdd expr)
     (cond ((eq (simplify (cadr expr)) (simplify (caddr expr))) (list '* (simplify (cadr expr)) 2))
           ((numberp (simplify (cadr expr))))))))
  
;; The following functions are inspired from the book Paradigms of Artificial Intelligence Programming on Artificial Intelligence.

;;Return a list of the factors of expression^n,  where each factor is of the form (^ y n).
(defun factorize (expression)
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x) (setf constant (* constant (expt x n))))
           ((beginsWith x '*)
                (fac (cadr x) n)
                (fac (caddr x) n)
           )
           ((beginsWith x '/)
                (fac (cadr x) n)
                (fac (caddr x) (- n))
                
                )
           ((and (beginsWith x '-) (and (consp (cdr x)) (null (rest (cdr x)))))
                (setf constant (- constant))
                (fac (cadr x) n)
           )
           ((and (beginsWith x '^) (numberp (caddr x)))
            (fac (cadr x) (* n (caddr x))))
            
           (t (let ((factor (find x factors :key #'cadr :test #'equal)))
                (if factor
                    (incf (caddr factor) n)
                    (push `(^ ,x ,n) factors)))))))
      
      (fac expression 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))
        
;; Divide a list of factors by another, producing a third.
 
(defun divide-factors (numer denom)
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (cadr d) result :key #'cadr :test #'equal)))
        (if factor
            (decf (caddr factor) (caddr d))
            (push `(^ ,(cadr d) ,(- (caddr d))) result))))
    (delete 0 result :key #'caddr)))

;;Partitions it into 2 lists - One that satisfies the pred and one that does not!
(defun partition-if (pred lst)
 (let ((yes-list nil)
        (no-list nil))
    (dolist (item lst)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

