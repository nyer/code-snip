;;;
;;; programs from the little schemer
;;;

;; check if x is a atom
(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

;; check if x is composited only of atom
(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
       (else #f))))

;; check if a is a member of lat 
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

;; build a list with the first occurence 
;; of the atom in the old list removed
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

;; takes an argument, a list, which is either a null list or contains
;; only none-empty lists. It builds another list composited of 
;; the first S-expression of each interal list.
(define firsts 
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((null? (car l)) (firsts (cdr l)))
      (else (cons 
              (car (car l))
              (firsts (cdr l)))))))

;; takes an argument, a list, which is either a null list or contains
;; only none-empty lists. It builds another list composited of 
;; the second S-expression of each interal list.
(define seconds 
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((null? (car l)) (seconds (cdr l)))
      (else (cons (car (cdr (car l)))
                  (seconds (cdr l)))))))

;; takes three arguments: the atoms new and old, and a list.
;; build a new lat with new inserted to the right of the first occurence
;; of old.
(define insertR
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

;; takes three arguments: the atoms new and old, and a list.
;; build a new lat with new inserted to the left of the first occurence
;; of old.
(define insertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

;; build a list with the first occurence of old replaced with new.
(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

;; build a list with the first occurence of o1 or o2 replaced with new.
(define subst2 
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? (car lat) o1)(eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

;; build a list with all occurences of a removed.
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

;; takes three arguments: the atoms new and old, and a list.
;; build a new lat with new inserted to the right of all  occurences
;; of old.
(define multiinsertR
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old) 
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;; takes three arguments: the atoms new and old, and a list.
;; build a new lat with new inserted to the left of all occurences
;; of old.
(define multiinsertL
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old)
       (cons new (multiinsertL new old (cdr lat))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;; build a list with all occurences of old replaced with new.
(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define +
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (add1 (+ m (sub1 n)))))))

(define -
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else (sub1 (- m (sub1 n)))))))
;; build a number by totaling all the numbers in its argument 
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

;; build a number by adding m up n times.
(define *
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else (+ m (* m (sub1 n)))))))

;; add the first number of tup1 to the first number of tup2,
;; then add second number of tup1 to the second number of tup2,
;; and so on, building a tup of answers, for tups of the same length.
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons 
              (+ (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (> (sub1 m) (sub1 n))))))

(define <
  (lambda (m n)
    (cond 
      ((zero? n) #f)
      ((zero? m) #t)
      (else (< (sub1 m) (sub1 n))))))

(define =
  (lambda (m n)
    (cond
      ((> m n) #f)
      ((< m n) #f)
      (else #t))))

(define expt1
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (* m (expt1 m (sub1 n)))))))

(define quotient1
  (lambda (m n)
    (cond 
      ((< m n) 0)
      (else (add1 (quotient1 (- m n) n))))))

(define length1
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length1 (cdr lat)))))))

(define pick
  (lambda n lat 
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick 
  (lambda n lat
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) 
                  (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((number? (car lat))
               (no-nums (cdr lat)))
              (else (cons (car lat)
                          (no-nums
                            (cdr lat)))))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond 
              ((number? (car lat))
               (cons (car lat)
                     (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) #t)
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur 
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond 
              ((eq? (car lat) a)
               (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
      (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((one? n) (cdr lat))
              (else (cons (car lat)
                          (rempick (sub1 n)
                                   (cdr lat)))))))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     (else
       (cond
	((eq? (car l) a) (rember* a (cdr l)))
	((atom? (car l)) (cons (car l) (rember* a (cdr l))))
	(else (cons (rember* a (car l))
		    (rember* a (cdr l)))))))))


(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
		  (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l)0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a) (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (+ (occur* a (car l))
	      (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
	(cons new (subst* new old (cdr l))))
       (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l))
		 (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car old))
	(cons new (cons old (insertL* new old (cdr l)))))
       (else (insertL* new old (cdr l)))))
     (else (cons (insertL* new old (car l))
		 (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (or (eq? (car l) a)
	  (member a (cdr l))))
     (else (or (member* a (car l))
		(member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     ((and (atom? (car l1))
	   (atom? (car l2))
	   (cond
	    ((eq? (car l1) (car l2)) 
	     (eqlist? (cdr l1) (cdr l2)))
	    (else #f))))
     ((or (atom? (car l1))
	  (atom? (car l2))) #f)
     (else
      (and (eqlist? (car l1) (car l2))
	   (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eq? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else (eqlist? (cdr s1) (cdr s2))))))

;rewrite eqlist? with equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and (equal? (car l1) (car l2))
		(eqlist? (cdr l1) (cdr l2)))))))

;simplify the remeber
(define rember
  (lambda (s l)
    (cond
     ((null? l) (quote ()))
     ((equal? s (car l)) (cdr l))
     (else (cons (car l)
		 (rember s (cdr l)))))))

# determin whether a representation of arithmetic expression
# contains only numbers besides +, * and ^
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp))
	   (numbered? (car (cdr (cdr aexp)))))))))

(define power
  (lambda (a b)
    (cond
     ((eq? b 1) a)
     (else (* a (power a (sub1 b)))))))

# eval the in-fix numbered arithmetic expression
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) (quote *))
      (* (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote +))
      (+ (value (car nexp))
	 (value (car (cdr (cdr nexp))))))
     (else 
      (power (value (car nexp))
	     (value (car (cdr (cdr nexp)))))))))

# eval the sufix numbered arithmetic expression.
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) (quote *))
      (* (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote +))
      (+ (value (car (cdr nexp)))
	 (value (car (cdr (cdr nexp))))))
     (else
      (power (value (car (cdr nexp)))
	     (value (car (cdr (cdr nexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
(define operator
  (lambda (aexp)
    (car aexp)))
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (+ (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote *))
      (* (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp))))
     (else
      (power (value (1st-sub-exp nexp))
	     (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))
(define edd1
  (lambda (n)
    (cons (quote ()) n)))
(define zub1
  (lambda (n)
    (cdr n)))
(define +
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (+ n (zub1 m)))))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     (else
      (not (or (member? (car lat) (cdr lat))
	       (set? (cdr lat))))))))

# cons the first atom in the lat onto the result 
# of natural recursion, after removing all the occurences
# of the first atom from the of the lat.
(define makeset
  (lambda (lat)
    (cond
     ((null? lat) lat)
     (else
      (cons (car lat)
	    (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else 
      (and (member? (car set1) set2)
	   (subset (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
	 (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else
      (or (member? (car set1) set2)
	  (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) (quote ()))
     ((member? (car set1) set2)
      (cons (car set1) (intersect (cdr set1) set2)))
     (else
      (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else
      (cons (car set1)
	    (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
		      (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     (else
      (not (null? (cdr (cdr x))))))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))
(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))
(define revrel
  (lambda (rel)
    (cond
     ((null? rel) (quote ()))
     (else
      (cons (revpair (car rel))
	    (revel (cdr rel))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) (quote ()))
     ((test? a (car l)) (cdr l))
     (else (cons (car l)
		 (rember-f (test? a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
		   ((rember-f test?) a
		    (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old)
	(cons new (cons old (cdr l))))
       (else
	(cons (car l)
	      ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((test? (car l) old)
	(cons new l))
       (else 
	(cons (car l)
	      ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))
(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) (quote ()))
       ((eq? (car l) old)
	(seq new old (cdr l)))
       (else (cons (car l)
		   ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g seqL))
(define insertR
  (insert-g seqR))

(define insertL
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst
  (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) +)
     ((eq? x (quote *)) *)
     (else power))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
	(operator nexp))
       (value (1st-sub-exp nexp))
       (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) (quote ()))
       ((test? (car lat) a)
	((multirember-f test?) (a (cdr lat))))
       (else
	(cons (car lat)
	      ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) (quote ()))
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
		 (multiremberT test? (cdr lat)))))))
