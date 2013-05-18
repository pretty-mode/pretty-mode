;; Ordering

(assert (<= 1 2))
(assert(<=(+ 1 1)3))

(assert (>= 3 2))
(assert(>=(+ 1 4)3))

;; Logic

(assert (and t t t))
(assert(and(and t)t t))

(assert (or nil nil t))
(assert(or(or nil) nil nil t))

(assert (not nil))
(assert(not'()))
(assert(not(not t)))

;; Sets

(assert (equal (intersection '(1 2 3) '(2 3 4)) '(3 2)))
(assert(equal(intersection'(1 2 3)'(2 3 4))'(3 2)))
(assert(equal(intersection(intersection'(1 2 3)'(1 2 3))'(2 3 4))'(3 2)))

(assert (equal (cl-intersection '(1 2 3) '(2 3 4)) '(3 2)))
(assert(equal(cl-intersection'(1 2 3)'(2 3 4))'(3 2)))
(assert(equal(cl-intersection(cl-intersection'(1 2 3)'(1 2 3))'(2 3 4))'(3 2)))

(assert (equal (union '(1 2 3) '(2 3 4)) '(4 1 2 3)))
(assert (equal(union'(1 2 3)'(2 3 4))'(4 1 2 3)))
(assert (equal(union(union'(1)'(2 3))'(2 3 4))'(4 1 2 3)))

(assert (equal (cl-union '(1 2 3) '(2 3 4)) '(4 1 2 3)))
(assert (equal(cl-union'(1 2 3)'(2 3 4))'(4 1 2 3)))
(assert (equal(cl-union(cl-union'(1)'(2 3))'(2 3 4))'(4 1 2 3)))

(assert (equal (set-difference '(1 2) '(2 3)) '(1)))
(assert(equal(set-difference'(1 2)'(2 3))'(1)))
(assert(equal(set-difference(set-difference'(1 2 3 4)'(3 4))'(2 3))'(1)))

(assert (equal (cl-set-difference '(1 2) '(2 3)) '(1)))
(assert(equal(cl-set-difference'(1 2)'(2 3))'(1)))
(assert(equal(cl-set-difference(cl-set-difference'(1 2 3 4)'(3 4))'(2 3))'(1)))

(assert (subsetp '(1 2) '(2 1 3)))
(assert(subsetp'(1 2)'(2 1 3)))
(assert(subsetp(union'(1)'(2))'(2 1 3)))

(assert (cl-subsetp '(1 2) '(2 1 3)))
(assert(cl-subsetp'(1 2)'(2 1 3)))
(assert(cl-subsetp(cl-union'(1)'(2))'(2 1 3)))

;; function

(assert ((lambda (x) x) t))
(assert((lambda(x)x)t))

;; nil

(assert (eq nil '()))
(assert(eq nil'()))

;; arithmetic

(assert (eql (sqrt 25) 5.0))
(assert(eql(sqrt(* 5 5))5.0))
