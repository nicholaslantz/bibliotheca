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
	   :explode))
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



(defun join-list (elts with &optional (firstp t) (acc nil))
  (if (null elts)
      (reverse acc)
      (if firstp
	  (join (cdr elts) with nil (list (car elts)))
	  (join (cdr elts) with nil (append (ensure-list (car elts))
					    (reverse (ensure-list with))
					    acc)))))

(defun split-list (lst with &optional (stack nil) (acc nil))
  "Split LST into sublists on boundaries WITH."
  (if (null lst)
      (if (consp stack)
	  (reverse (cons (reverse stack) acc))
	  (reverse acc))
      (if-let ((rst (beginsp lst with)))
	  (if (consp stack)
	      (split rst with nil (cons (reverse stack) acc))
	      (split rst with nil acc))
	  (split (cdr lst) with (cons (car lst) stack) acc))))

(defun split-list-if (lst test &optional (stack nil) (acc nil))
  (if (null lst)
      (if (consp stack)
	  (reverse (cons (reverse stack) acc))
	  (reverse acc))
      (if (funcall test (car lst))
	  (if (consp stack)
	      (split-if (cdr lst) test nil (cons (reverse stack) acc))
	      (split-if (cdr lst) test nil acc))
	  (split-if (cdr lst) test (cons (car lst) stack) acc))))

(defun range-int (start stop step &optional (acc nil))
  "Implementation of range"
  (if (< start stop)
      (if (>= start stop)
	  (reverse acc)
	  (range-int (+ start step) stop step (cons start acc)))
      (if (<= start stop)
	  (reverse acc)
	  (range-int (+ start step) stop step (cons start acc)))))

(defun range (start &optional (stop nil) (step nil))  
  "Produce list of numbers beginning from START (inclusive)
and ending at STOP (exclusive) incrementing by STEP."
  (cond ((and stop step) (range-int start stop step))
	(stop (range-int start stop (if (> stop start) 1 -1)))
	(t (range-int 0 start 1))))

(defun flatten (lst &optional (rev t) (acc nil))
  "Return all atoms in nested list LST in a non-nested list.

If REV is T, the returned list will be reversed"
  (cond ((null lst)
	 (if rev
	     acc
	     (reverse acc)))
	((atom (car lst))
	 (flatten (cdr lst) rev (cons (car lst) acc)))
	(t
	 (flatten (cdr lst) rev (append (flatten (car lst)) acc)))))

(defun take (lst &optional (n 1) (acc nil))
  (if (or (endp lst) (zerop n))
      (values (nreverse acc) lst)
      (take (cdr lst) (- n 1) (cons (car lst) acc))))

(defun drop (lst &optional (n 1))
  (multiple-value-bind (head tail)
      (take lst n)
    (values tail head)))

(defun group (lst n &optional (acc nil))
  (if (or (endp lst) (null (nthcdr (- n 1) lst)))
      (values (nreverse acc) lst)
      (multiple-value-bind (next rst)
	  (take lst n)
	(group rst n (cons next acc)))))

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
    (sort v #'string<)))

(defmacro lm (&body body)
  `(lambda ,(lm-vars body)
     ,@body))

(defun explode (sym)
  (mapcar (lm (intern (coerce $ 'string)))
	  (group (coerce (symbol-name sym) 'list) 1)))
