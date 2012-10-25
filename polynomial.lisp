(defun integrate (expr dvar)
  (cond
    ((notContainsVariable dvar expr) `(* ,expr x))
    ((numberp expr) (make-prod expr dvar))
    ((symbolp expr) (make-div (make-pow expr 2) 2))
    ((isPow expr dvar) 
     (make-div (make-pow dvar (+ (exponent expr) 1)) (+ (exponent expr) 1))
     )
    ((isAdd expr) (make-sum (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
    ((isUnaryMinus expr) `(- , (integrate (cadr expr) dvar) ))
    ((isSub expr)  (make-sub (integrate (cadr expr) dvar) (integrate (caddr expr) dvar)))
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
    ((isSin expr)
     (cond 
       ((eq dvar (cadr expr))(make-prod ('cos dvar) -1))
       ((isAx (cadr expr)) ())
           )
     
     )
    ((isCos expr)
     ('sin )
     )
    )
  )

(defun isAx (expr dvar)
  (and (eq '* (car expr)) 
       (or 
         (and (eq dvar (cadr expr)) (not (containsVariable dvar (caddr expr))))
         (and (eq dvar (caddr expr)) (not (containsVariable dvar (cadr expr))))
         )
       )
  )
(defun getA (expr dvar)
  (if ())
  )

(defun isSin (expr)
  (eq (car expr) 'sin)
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
    ((numberp expr) 0)
    ((symbolp expr) 1)
    ((isAdd expr) (make-sum (differentiate (cadr expr) dvar) (differentiate (caddr expr) dvar)))
    ((isProd expr) 
     (make-sum 
       (make-prod (multiplier expr) (differentiate (multiplicand expr) dvar))
       (make-prod (multiplicand expr) (differentiate (multiplier expr) dvar))
       )
     )
    ((isPow expr dvar) (make-prod (make-pow (base expr) (- (exponent expr) 1))(exponent expr)))
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

(defun starts-with ( list x )
    ( and (consp list ) (eql (first list) x ))

(defun factorize (expr)
  "Return a list of the factors of expr^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x) (setf constant (* constant (expr x n))))
           ((starts-with x '*) (fac ((cadr expr) x) n) (fac (e x) n))
           ((starts-with x '/)
            (fac ((cadr expr) x) n)
            (fac ((caddr expr) x) (- n)))
           ((and (starts-with x '-) (eq (length (rest 1)) 1))
            (setf constant (- constant))
            (fac ((cadr expr) x) n))
           ((and (starts-with x '^) (numberp ((caddr expr) x)))
            (fac ((cadr expr) x) (* n ((caddr expr) x))))
           (t (let ((factor (find x factors :key #'(cadr expr)
                                  :test #'equal)))
                (if factor
                    (incf ((caddr expr) factor) n)
                    (push `(^ ,x ,n) factors)))))))
      
      ;; Body of factorize:
      (fac exp 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))

(defun notContainsVariable (var expr)
   (not (find-Variable var expr))
  )

(defun find-Variable (var expr)
  (cond ((eql var expr) expr)
        ((atom expr) nil)
        ((find-Variable var (car expr)))
        ((find-Variable var (cdr expr)))
        )
  )
(defun inprint (expr)
  (if (atom expr) (print expr)
    (and (print (cadr expr)) (inprint (car expr)) (inprint (caddr expr)))
    )
  )

(defun simplify (expr)
  (cond 
    ((isAdd expr)
     (cond ((eq (simplify (cadr expr)) (simplify (caddr expr))) (list '* (simplify (cadr expr)) 2))
           ((numberp (simplify (cadr expr)) ))
           )
     )
    )
  
  )
