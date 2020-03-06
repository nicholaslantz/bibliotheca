(defpackage :bibliotheca
  (:use :cl :parachute)
  (:export :ensure-list
	   :if-let :if-not-let :and-let :or-let :when-let :unless-let
	   :beginsp :endsp :containsp
	   :join-list :split-list :split-list-if
	   :range
	   :flatten :zip
	   :take :drop :group
	   :assoc-default :assocdr :assocdr-if :assocdr-default
	   :concat :join-strings
	   :nth-wa
	   :choose
	   :clamp :lerp :invlerp :lmap
	   :score :best :bestk
	   :clearf
	   :~> :~>> :as~>
	   :read-lines-until
	   :strip-left :strip-right :strip
	   :lm
	   :call-with-bindings :with-bindings
	   :explode :unexplode))
(in-package :bibliotheca)

(defun ensure-list (elt)
  "If ELT is a list it is returned unmodified.  Otherwise it is
wrapped in a list."
  (if (listp elt)
      elt
      (list elt)))

(define-test ensure-list
  (of-type list (ensure-list 5))
  (of-type list (ensure-list nil))
  (of-type list (ensure-list (cons 1 nil)))
  (of-type list (ensure-list '(1 2 3)))

  (is equal '(5) (ensure-list 5))
  (is equal '() (ensure-list nil))
  (is equal '(1) (ensure-list (cons 1 nil)))
  (is equal '(1 2 3) (ensure-list '(1 2 3))))

;; XXX: How do we test macros?  For these we need to check that the
;; bindings are bound and then the path is chosen appropriately.

(defmacro if-let (binding then &optional (else nil))
  `(let ,binding
     (if ,(caar binding)
	 ,then
	 ,else)))

(defmacro if-not-let (binding then &optional (else nil))
  `(let ,binding
     (if (not ,(caar binding))
	 ,then
	 ,else)))

(defmacro and-let (bindings then &optional (else nil))
  `(let ,bindings
     (if (and ,@(mapcar #'car bindings))
	 ,then
	 ,else)))

(defmacro or-let (bindings then &optional (else nil))
  `(let ,bindings
     (if (or ,@(mapcar #'car bindings))
	 ,then
	 ,else)))

(defmacro when-let (bindings &body then)
  `(let ,bindings
     (when ,(caar bindings)
       ,@then)))

(defmacro unless-let (bindings &body then)
  `(let ,bindings
     (unless ,(caar bindings)
       ,@then)))

(defun beginsp (lst with)
  "If LST begins with WITH return the rest of it, otherwise NIL.

Note: if LST and WITH are EQUAL, nil is returned."
  (let ((with (ensure-list with)))
    (when (and (nthcdr (length with) lst)
	       (equal (subseq lst 0 (length with)) with))
      (subseq lst (length with)))))

(define-test beginsp
  (true (beginsp '(1 2 3) '(1)))
  (true (beginsp '(1 2 3) '(1 2)))
  (is equal nil (beginsp '(1 2 3) '(1 2 3)))

  (true (beginsp '(1 2 3) 1))

  (false (beginsp '(4 5 6) '(5 6))))

(defun endsp (lst with)
  "If LST ends with WITH, return the the beginning of it, otherwise NIL.

Note: if LST and WITH are EQUAL, nil is returned."
  (let ((with (ensure-list with)))
    (when (and (nthcdr (length with) lst)
	       (equal (subseq lst (- (length lst) (length with))) with))
      (subseq lst 0 (- (length lst) (length with))))))

(define-test endsp
  (true (endsp '(1 2 3) '(3)))
  (true (endsp '(1 2 3) '(2 3)))
  (is equal nil (endsp '(1 2 3) '(1 2 3)))

  (fail (endsp '(1 2 3) 3))

  (false (endsp '(4 5 6) '(4 5))))

;; TODO: Add a COUNT keyword-arg, like remove family of functions.
(defun containsp (lst sublst)
  "If LST contains at least 1 instance of SUBLST, return the rest of
LST after the first occurence, otherwise NIL.

Note: if LST and SUBLST are EQUAL or if LST ends with SUBLST, nil
is returned."
  (cond ((null lst) nil)
	((beginsp lst sublst)
	 (if (= (length lst) (length sublst))
	     t
	     (subseq lst (length sublst))))
	(t (containsp (cdr lst) sublst))))

(define-test containsp
  (true (containsp '(1 2 3 4 5) '(2 3 4)))
  (true (containsp '(1 2 3 4 5) '(1 2 3 4)))
  (true (containsp '(1 2 3 4 5) '(3)))
  
  (is eql nil (containsp '(1 2 3 4 5) '(5)))

  (fail (containsp '(1 2 3 4 5) 3) type-error))

(defun join-list (elts with &optional (acc nil))
  "Return a list containing ELTS with WITH between each of them.

e.g. (join-list '(a b c) '(0 1)) -> '(a 0 1 b 0 1 c)"
  (if (null elts)
      (reverse acc)
      (if (null acc)
	  (join-list (cdr elts) with (list (car elts)))
	  (join-list (cdr elts) with (append (list (car elts))
					     (reverse (ensure-list with))
					     acc)))))

(define-test join-list
  (is equal '(1 2 1) (join-list '(1 1) '(2)))
  (is equal '(1 0 1 0 1 0 1) (join-list '(1 1 1 1) 0))
  (is equal '(a nil b) (join-list '(a b) '(nil)))

  (is equal '(a b) (join-list '(a b) nil)))

(defun split-list (lst with &optional (stack nil) (acc nil))
  "Split LST into sublists on boundaries WITH."
  (if (null lst)
      (if (consp stack)
	  (reverse (cons (reverse stack) acc))
	  (reverse acc))
      (if-let ((rst (beginsp lst with)))
	  (if (consp stack)
	      (split-list rst with nil (cons (reverse stack) acc))
	      (split-list rst with nil acc))
	  (split-list (cdr lst) with (cons (car lst) stack) acc))))

(defun split-list-if (lst test &optional (stack nil) (acc nil))
  "Split LST into SUBLSTS when TEST is true for the element being
examined."
  (if (null lst)
      (if (consp stack)
	  (reverse (cons (reverse stack) acc))
	  (reverse acc))
      (if (funcall test (car lst))
	  (if (consp stack)
	      (split-list-if (cdr lst) test nil (cons (reverse stack) acc))
	      (split-list-if (cdr lst) test nil acc))
	  (split-list-if (cdr lst) test (cons (car lst) stack) acc))))

;; FIXME: I think the -range* functions can be merged with some lambda
;;        functions.  Merge if possible.

(defun -range-inc (start stop step &optional (acc nil))
  (if (>= start stop)
      (nreverse acc)
      (-range-inc (+ start step) stop step (cons start acc))))

(defun -range-dec (start stop step &optional (acc nil))
  (if (<= start stop)
      (nreverse acc)
      (-range-dec (+ start step) stop step (cons start acc))))

(defun -range (start stop step)
  (if (< start stop)
      (-range-inc start stop step)
      (-range-dec start stop step)))

(defun range (start &optional (stop nil) (step nil))  
  "Produce list of numbers beginning from START (inclusive)
and ending at STOP (exclusive) incrementing by STEP."
  (cond ((and stop step) (-range start stop step))
	(stop (-range start stop (if (> stop start) 1 -1)))
	(t (-range 0 start 1))))

(define-test range
  (is equal '(0 1 2 3) (range 4))
  (is equal '(-2 -1 0 1 2) (range -2 3))
  (is equal '(10 5 0) (range 10 -1 -5))
  (is equal 100 (length (range 100)))
  (is equal '(5 4 3 2 1) (range 5 0)))

(defun flatten (lst &optional (rev t) (acc nil))
  "Return all atoms in nested list LST in a single-dimensional list.

If REV is nil, the returned list will not be reversed.  It is t by
default."
  (cond ((null lst) (if rev (nreverse acc) acc))
	((atom (car lst)) (flatten (cdr lst)
				   rev
				   (cons (car lst) acc)))
	(t (flatten (cdr lst)
		    rev
		    (append (flatten (car lst) nil) acc)))))

(define-test flatten
  (is equal '(1 2 3) (flatten '(1 2 3)))
  (is equal '(3 2 1) (flatten '(1 2 3) nil))
  (is equal '(1 2 3) (flatten '(1 (2 (3)))))
  (is equal '(3 2 1) (flatten '(3 (2 (1)))))
  (is equal '(3 2 1) (flatten '(1 (2 3)) nil))

  (is equal
      '(-1 0 1 2 3 4 5 6 7 8 9)
      (flatten '((-1 0) (1) (2 (3 (4 5) (6 7) (8 (9)))))))
  (is equal
      '(9 8 7 6 5 4 3 2 1 0 -1)
      (flatten '((-1 0) (1) (2 (3 (4 5) (6 7) (8 (9))))) nil)))

(defun take (lst &optional (n 1) (acc nil))
  "Take N items from the front of LST."
  (cond ((and (plusp n) (endp lst)) (error "Cannot take ~A from empty list" n))
	((zerop n) (values (nreverse acc) lst))
	(t (take (cdr lst) (- n 1) (cons (car lst) acc)))))

(define-test take
  (is-values (take '(1 2 3))
    (equal '(1))
    (equal '(2 3)))
  (is-values (take '(1 2 3) 2)
    (equal '(1 2))
    (equal '(3)))
  (is-values (take '(1 2) 2)
    (equal '(1 2))
    (equal '()))
  (fail (take '()))
  (fail (take '(1) 2)))

(defun drop (lst &optional (n 1))
  "Drop N items from the front of LST."
  (multiple-value-bind (head tail)
      (take lst n)
    (values tail head)))

(define-test drop
  (is-values (drop '(1 2 3))
    (equal '(2 3))
    (equal '(1)))
  (is-values (drop '(1 2 3) 2)
    (equal '(3))
    (equal '(1 2)))
  (is-values (drop '(1 2) 2)
    (equal '())
    (equal '(1 2)))
  (fail (drop '()))
  (fail (drop '(1) 2)))

(defun group (lst n &optional (acc nil))
  "Return lists of length N made of consecutive elements of LST.  If
LST's length is not a multiple of N, excess items are not returned."
  (if (or (endp lst) (null (nthcdr (- n 1) lst)))
      (values (nreverse acc) lst)
      (multiple-value-bind (next rst)
	  (take lst n)
	(group rst n (cons next acc)))))

(define-test group
  (is equal '((1 2) (3 4)) (group '(1 2 3 4) 2))
  (is equal '((1) (2) (3)) (group '(1 2 3) 1))
  (is equal '((1 2) (3 4)) (group '(1 2 3 4 5) 2))
  (fail (group '(1 2 3) 0))
  (is equal '() (group '() 2))
  (is-values (group '(1 2 3) 2)
    (equal '((1 2)))
    (equal '(3))))

(defun zip (&rest lsts)
  (reduce (lambda (a b)
	    (mapcar #'cons a b))
	  lsts
	  :initial-value (make-list
			  (length (car lsts))
			  :initial-element nil)
	  :from-end t))

(defun assoc-default (item alist default)
  "Assoc ITEM in ALIST, return DEFAULT if not found."
  (if-let ((cell (assoc item alist)))
    cell
    default))

(defun assocdr (item alist &key (test #'eql))
  (cdr (assoc item alist :test test)))

(defun assocdr-if (predicate alist)
  (cdr (assoc-if predicate alist)))

(defun assocdr-default (item alist default)
  (if-let ((cell (assoc item alist)))
    (cdr cell)
    default))

(defun concat (&rest strings)
  (apply #'concatenate (append '(string) strings)))

(defun join-strings (strs with &optional (acc ""))
  (if (null (cdr strs))
      (concat acc (car strs))
      (join-strings (cdr strs) with (concat acc (car strs) with))))

(defun nth-wa (n lst)
  "NTH, but N can be negative and longer than length of LST. It Wraps Around.

Similar to indexing in Python."
  (nth (mod n (length lst)) lst))

(defun choose (lst &optional (n 1))
  "Take N uniforamally-distributed samples from LST with replacement."
  (let ((res nil)
	(len (length lst)))
    (dotimes (i n)
      (push (nth (random len) lst) res))
    (if (= n 1)
	(car res)
	res)))

(defun clamp (v a b)
  (max a (min v b)))

(defun lerp (v a b)
  (+ a (* v (- b a))))

(defun invlerp (x a b)
  (/ (- x a) (- b a)))

(defun lmap (x a b p q)
  (lerp (invlerp x a b) p q))

(defun score (data fn &key (key #'identity))
  (mapcar #'cons data (mapcar fn (mapcar key data))))

(defun best (data fn &key (predicate #'>) (key #'identity))
  (reduce (lambda (acc n)
	    (if (funcall predicate (cdr n) (cdr acc)) n acc))
	  (score data fn :key key)))

(defun bestk (data k fn &key (predicate #'>) (key #'identity))
  (let ((res nil)
	(working-data data))
    (dotimes (i k res)
      (push (best working-data fn :predicate predicate :key key)
	    res)
      (setf working-data (remove (caar res) working-data)))))

(defmacro clearf (place)
  `(setf ,place nil))

(defmacro ~> (form &body fns)
  (if (endp fns)
      form
      `(~> ,(append (list (caar fns)) (list form) (cdar fns)) ,@(cdr fns))))

(defmacro ~>> (form &body fns)
  (if (endp fns)
      form
      `(~>> ,(append (car fns) (list form)) ,@(cdr fns))))

(defmacro as~> (as form &body fns)
  (if (endp fns)
      form
      `(as~> ,as ,(substitute form as (car fns)) ,@(cdr fns))))

(defun read-lines-until (s test &optional (acc nil))
  (let ((line (read-line s)))
    (if (funcall test line)
	(reverse acc)
	(read-lines-until s test (cons line acc)))))

(defun strip-left (line &optional (what '(#\Return #\Newline #\Space)))
  (if (member (car line) what)
      (strip-left (cdr line) what)
      line))

(defun strip-right (line &optional (what '(#\Return #\Newline #\Space)))
  (nreverse (strip-left (reverse line) what)))

(defun strip (line &optional (what '(#\Return #\Newline #\Space)))
  (strip-right (strip-left line what) what))

(defun lm-vars (form)
  (as~> v form
    (flatten v)
    (remove-if-not #'symbolp v)
    (mapcar #'symbol-name v)
    (remove-if-not (lambda (s) (member s '("$" "$1" "$2" "$3" "$4")
				       :test #'string=))
		   v)
    (mapcar #'intern v)
    (remove-duplicates v)
    (sort v #'string<)))

;; FIXME: lm does not work when it is nested in another lm.
;;
;; That could be desirable as its meant for quick and easy functions.
;; In that case, it should signal an error when it is nested.
;;
;; The error is that the lm-vars function captures the variable names
;; inside of inner lm functions.  This causes the outer lambda
;; functions to have more variables in their lambda-lists than they
;; use, causing an invalid number of arguments error.
(defmacro lm (&body body)
  `(lambda ,(lm-vars body)
     ,@body))

;; Both functions below inspired by Swank
;; https://github.com/slime/slime/blob/1761172817d2e1a8b48c216bf0d261eb277ae562/swank.lisp

(defun call-with-bindings (alist fun)
  (if (null alist)
      (funcall fun)
      (let* ((rlist (reverse alist))
	     (vars (mapcar #'car rlist))
	     (vals (mapcar #'cdr rlist)))
	(progv vars vals
	  (funcall fun)))))

(defmacro with-bindings (alist &body body)
  "Bind ALIST to the local lexical environment of BODY."
  `(call-with-bindings ,alist (lambda () ,@body)))

(defun explode (sym)
  (mapcar (lm (intern (coerce $ 'string)))
	  (group (coerce (symbol-name sym) 'list) 1)))

(defun unexplode (syms)
  (intern (apply #'concat (mapcar (lm (symbol-name $)) syms))))
