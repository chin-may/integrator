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
    ;;  Call the one that contains x as x-factors and the one that does not as const-factors
    
    ((multiple-value-bind (const-factors x-factors)
       (partition-if #'(lambda (factor) (notContainsVariable factor dvar))
                       (factorize expr))
       ;; Now integrate and return
       `(* 
             ,(unfactorize const-factors)                   
             
             ,(cond ((null x-factors) dvar)                     ;; If there are no factors containing variable return var
                    
                    ;; Try to integrate and check if the derivative divides the integral or other such forms
                    ((some #'(lambda (factor) (deriv-divides factor x-factors dvar))
                           x-factors))                          
                           
                    ;; u-v rule for integration!
                    ((isProd expr) 
                         (cond 
                           ((numberp (cadr expr)) (make-prod (cadr expr) (integrate (caddr expr) dvar)))
                           ((numberp (caddr expr)) (make-prod (caddr expr) (integrate (cadr expr) dvar)))
                           (t (make-sub
                                (make-prod (caddr expr) (integrate (cadr expr) dvar))
                                (integrate 
                                  (make-prod 
                                    (integrate (cadr expr) dvar) (differentiate (caddr expr) dvar)) dvar)))
                         ))
                   c
                    (t `(INTEGRATE ,(unfactorize x-factors) ,dvar))))))
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
     `(- (log (cos ,expr)))
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
    
    ((isExp expr) (handleExp expr dvar))
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
    ((eq dvar (caddr expr) )
     `(+ (^ e ,dvar) c)
     )
    ((isAx (caddr expr) dvar)
     `(+ ,(make-div `(^ e ,(caddr expr))  (getA (caddr expr) dvar)) c)
     )
    ((isXb (caddr expr) dvar)
     `(+ (^ e , (caddr expr)) c)
     )
    ((isAxb (caddr expr) dvar)
     `(+ ,(make-div `(^ e ,(caddr expr))  (getA (caddr expr) dvar)) c)
     )
    ((or (isXbyA (caddr expr) dvar) (isXbyAplusb (caddr expr) dvar))
     `(+ ,(make-prod `(^ e ,(caddr expr))  (getA (caddr expr) dvar)) c)
     )
    )
  )

(defun handleAExp (expr dvar)
  (cond
    ((eq dvar (caddr expr) )
     `(+ (/ (^ ,(cadr expr) ,dvar) (log ,(cadr expr))) c)
     )
    ((isAx (caddr expr) dvar)
     `(+ (/ ,(make-div `(^ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr))) c)
     )
    ((isXb (caddr expr) dvar)
     `(+ (/ (^ ,(cadr expr) ,(caddr expr)) (log ,(cadr expr))) c)
     )
    ((isAxb (caddr expr) dvar)
     `(+ (/ ,(make-div `(^ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr))) c)
     )
    ((or (isXbyA (caddr expr) dvar) (isXbyAplusb (caddr expr) dvar))
     `(+ (/ ,(make-prod `(^ ,(cadr expr) ,(caddr expr))  (getA (caddr expr) dvar)) (log ,(cadr expr))) c)
     )
    )
  )
  

(defun partition-if (pred lst)
"Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item lst)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

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
  (and (eq (car expr) '^) (eq (cadr expr) 'e))))

(defun isAExp (expr dvar)
  (if (atom expr) nil
  
  (and (eq (car expr) '^) (notContainsVariable (cadr expr) dvar))))

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

(defun starts-with ( lst x )
    ( and (consp lst ) (eql (first lst) x )))

;; This factorization function is based on PAIP.
(defun factorize (expression)
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x)
            (setf constant (* constant (expt x n))))
           ((starts-with x '*)
            (fac (cadr x) n)
            (fac (caddr x) n))
           ((starts-with x '/)
            (fac (cadr x) n)
            (fac (caddr x) (- n)))
           ((and (starts-with x '-) (and (consp (cdr x)) (null (rest (cdr x)))
            (setf constant (- constant))
            (fac (cadr x) n))))
           ((and (starts-with x '^) (numberp (caddr x)))
            (fac (cadr x) (* n (caddr x))))
           (t (let ((factor (find x factors :key #'cadr
                                  :test #'equal)))
                (if factor
                    (incf (caddr factor) n)
                    (push `(^ ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac expression 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))
        

(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((and (consp factors) (null (cdr factors))) (first factors))
        (t `(* ,(first factors) ,(unfactorize (rest factors))))))

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
  
(defun deriv-divides (factor factors x)
  (let* ((u (cadr factor))              ; factor = u^n
         (n (caddr factor))
         (k (divide-factors 
              factors (factorize `(* ,factor ,(differentiate u x))))))
    (cond ((notContainsVariable k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n/=1
           ;;                    = k*log(u) for n=1
           (if (= n -1)
               `(* ,(unfactorize k) (log ,u))
               `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((and (= n 1) (or (isSin u) (isCos u) (isTan u) (isLog u) (isexp u) (isAexp u x)) )
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k2 (divide-factors
                       factors
                       (factorize `(* ,u ,(differentiate (cadr u) x))))))
             (if (notContainsVariable k2 x)
                 `(* ,(integrateFromTable u x)
                     ,(unfactorize k2))))))))
  
(defun inprint (expr)
  (cond ((atom expr) expr)
    ((eql (length expr) 2) expr)
    (t (list (inprint (cadr expr)) (inprint (car expr)) (inprint (caddr expr))))
    )
  )

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (cadr d) result :key #'cadr :test #'equal)))
        (if factor
            (decf (caddr factor) (caddr d))
            (push `(^ ,(cadr d) ,(- (caddr d))) result))))
    (delete 0 result :key #'caddr)))

;; TODO : Need to simplify expressions
(defun simplify (expr)
  (cond 
    ((isAdd expr)
     (cond ((eq (simplify (cadr expr)) (simplify (caddr expr))) (list '* (simplify (cadr expr)) 2))
           ((numberp (simplify (cadr expr))))))))

