(defun integrate (expr dvar)
  (cond
    ((notContainsVariable expr dvar ) `(* ,expr x))
    ((numberp expr) (make-prod expr dvar))
    ((symbolp expr) (make-div (make-pow expr 2) 2))
    ((isPow expr dvar) 
     (make-div (make-pow dvar (+ (exponent expr) 1)) (+ (exponent expr) 1))
     )
    ((isAdd expr) (make-sum (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
    ((isUnaryMinus expr) `(- , (integrate (cadr expr) dvar) ))
    ((isSub expr)  (make-sub (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
    ((isSin expr)
     (cond 
       ((eq dvar (cadr expr)) 
        `(+ (- (cos ,dvar)) c)
        )
       ((isAx (cadr expr) dvar) 
        `(+ (- ,(make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar))) c)
        )
       ((isXb (cadr expr) dvar)
        `(+ (- (cos ,(cadr expr))) c)
        )
       ((isAxb (cadr expr) dvar)
        `(+ (- ,(make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar))) c)
        )
       )
     )
     
    ((isCos expr)
     (cond 
       ((eq dvar (cadr expr)) 
        `(+ (sin ,dvar)  c)
        )
       ((isAx (cadr expr) dvar) 
        (list '+ (make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar)) 'c)
        )
       ((isXb (cadr expr) dvar)
        `(+ (sin ,(cadr expr)) c)
        )
       ((isAxb (cadr expr) dvar)
        (list '+ (make-div (list 'cos (cadr expr)) (getA (cadr expr) dvar)) 'c)
        )
       )
     )
    ((isExp expr) (handleExp expr dvar))
    
    ((multiple-value-bind (const-factors x-factors)
         (partition-if #'(lambda (factor) (notContainsVariable factor dvar))
                       (factorize expr))
       (identity
         `(* ,(unfactorize const-factors)
             ;; And try to integrate:
             ,(cond ((null x-factors) dvar)
                    ((some #'(lambda (factor)
                               (deriv-divides factor x-factors dvar))
                           x-factors))
                    (t `(integrate ,(unfactorize x-factors) ,dvar)))))))
     
    ((isProd expr)
     (cond 
       ((numberp (cadr expr)) (make-prod (cadr expr) (integrate (caddr expr) dvar)))
       ((numberp (caddr expr)) (make-prod (caddr expr) (integrate (cadr expr) dvar)))
       (T (make-sub
            (make-prod (caddr expr) (integrate (cadr expr) dvar))
            (integrate 
              (make-prod 
                (integrate (cadr expr) dvar) (differentiate (caddr expr) dvar)) dvar)))
       )
     )
     
    ((isDiv expr)
     (if (numberp (divisor expr))
       (make-div (integrate (dividend expr) dvar) (divisor expr))
       )
     )
    )
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
     `(+ ,(make-div `(^ e ,(caddr expr))  (getA (caddr expr)) dvar) c)
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
 (and (eq (car expr) '+ )
      (or 
        (and (eq dvar (cadr expr)) (notContainsVariable (caddr expr) dvar))
        (and (eq dvar (caddr expr)) (notContainsVariable (cadr expr) dvar))
        )
      ) 
  )

(defun isAxb (expr dvar)
  (and (eq '+ (car expr)) 
       (or (and (isAx (cadr expr) dvar) (notContainsVariable (caddr expr) dvar ))
           (and (isAx (caddr expr) dvar) (notContainsVariable (cadr expr) dvar ))
           )
       )
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

(defun isExp (expr)
  (and (eq (car expr) '^) (eq (cadr expr) 'e)))

(defun isAExp (expr)
  (and (eq (car expr) '^) (notContainsVariable (cadr expr))))

(defun isLog (expr)
  (eq (car expr) 'log)
  )

(defun isSin (expr)
  (eq (car expr) 'sin)
  )

(defun isCos (expr)
  (eq (car expr) 'cos)
  )

(defun isDiv (expr)
  (eq '/ (car expr))
  )

(defun isPow (expr dvar)
  (and (eq (car expr) '^) (eq (cadr expr) dvar) (numberp (caddr expr)))
  )

(defun isAdd (expr)
  (eq '+ (car expr)))

(defun isProd (expr)
  (eq '* (car expr)))

(defun isUnaryMinus (expr)
  (and (eq '- (car expr)) (eq (length expr) 2))
  )

(defun isSub (expr)
  (and (eq '- (car expr)) (eq (length expr) 3))
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
    ((isPow expr dvar) (make-prod (make-pow (base expr) (- (exponent expr) 1))(exponent expr)))
    ((isSin expr) (make-prod (list 'cos (cadr expr)) (differentiate (cadr expr) dvar)))
    ((isCos expr) (make-prod (list '- (list 'sin (cadr expr))) (differentiate (cadr expr) dvar)))
    ((isExp expr) (make-prod expr (differentiate (caddr expr) dvar)))
    ((isAExp expr) (make-prod (list 'log (cadr expr)) (make-prod expr (differentiate (caddr expr) dvar)) ))
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
    ((eq 0 l) (- r)) 
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

(defun factorize (expr)
  "Return a list of the factors of expr^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x) (setf constant (* constant (expt x n))))
           ((starts-with x '*) (fac (cadr  x) n) (fac (caddr x) n))
           ((starts-with x '/)
            (fac (cadr x) n)
            (fac (caddr x) (- n)))
           ((and (starts-with x '-) (eq (length (rest 1)) 1))
            (setf constant (- constant))
            (fac (cadr x) n))
           ((and (starts-with x '^) (numberp (caddr x)))
            (fac (cadr x) (* n (caddr x))))
           (t (let ((factor (find x factors :key #'cadr
                                  :test #'equal)))
                (if factor
                    (incf (caddr factor) n)
                    (push `(^ ,x ,n) factors)))))))
      
      (fac expr 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `(,constant .,factors))))))


(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((eq (length factors) 1) (first factors))
        (t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun notContainsVariable (expr var)
  (if (atom expr)
    (not(eq expr var))
    (not (find-Variable var expr))
    )
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
         (divfacts (divide-factors 
              factors (factorize `(* ,factor ,(differentiate u x))))))
    (cond ((notContainsVariable divfacts x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n<>1
           ;;                    = k*log(u) for n=1
           (if (= n -1)
               `(* ,(unfactorize divfacts) (log ,u))
               `(/ (* ,(unfactorize divfacts) (^ ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((= n 1)
           ;; TODO : Check if u is actually integrable 
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k (divide-factors
                       factors
                       (factorize `(* ,u ,(differentiate (cadr u) x))))))
             (if (notContainsVariable k x)
                 `(* ,(integrate (car u) (cadr u))
                     ,(unfactorize k))))))))
  
(defun inprint (expr)
  (if (atom expr) (print expr)
    (and (print (cadr expr)) (inprint (car expr)) (inprint (caddr expr)))
    )
  )

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (cadr d) result :key #'cadr
                          :test #'equal)))
        (if factor
            (decf (caddr factor) (caddr d))
            (push `(^ ,(cadr d) ,(- (caddr d))) result))))
    (delete 0 result :key #'caddr)))
  

(defun simplify (expr)
  (cond 
    ((isAdd expr)
     (cond ((eq (simplify (cadr expr)) (simplify (caddr expr))) (list '* (simplify (cadr expr)) 2))
           ((numberp (simplify (cadr expr))))))))
