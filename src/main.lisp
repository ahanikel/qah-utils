(defpackage qah-utils
  (:use :cl)
  (:export :-> :->> :doto :_
           :filter :select :trim-whitespace :map-n
           :ensure :key :with :foreach
           :safe-encode :safe-decode
           :last1 :singlep :append1 :nconc1 :mklist :longer :group
           :flatten :prune :find2 :before :after :duplicate :split-if))

(in-package #:qah-utils)

(defun make-object ()
  (make-hash-table :test 'equalp))

;;-----------------------------------------------------------------------------
;; We want the same luxury as clojure...
;;-----------------------------------------------------------------------------
(defmacro -> (&body body)
  "Evaluates the first expression of BODY, then inserts the result as
   the first argument of the second expression, evaluates that, and so on.
   The evaluation short-circuits as soon as an expression evaluates to NIL.
   Returns the value of the modified first expression."
  (let ((tmp (gensym))
        (first-expr (gensym)))
    `(let ((,first-expr ,(car body)))
       ,(append
         (do* ((b      body
                       (cdr b))
               (expr   first-expr
                       (car b))
               (res    expr
                       `(let ((,tmp ,res))
                          (when ,tmp
                            ,(cons (car expr)
                                   (cons tmp (cdr expr)))))))
              ((null (cdr b)) res))
         (list first-expr)))))

(defmacro ->. (&body body)
  "Evaluates the first expression of BODY, then inserts the result as
   the first argument of the second expression, evaluates that, and so on.
   The evaluation short-circuits as soon as an expression evaluates to NIL.
   Returns the value of the last expression."
  (let ((tmp (gensym)))
    (do* ((b      body
                  (cdr b))
          (expr   (car b)
                  (car b))
          (res    expr
                  `(let ((,tmp ,res))
                     (when ,tmp
                       ,(cons (car expr)
                              (cons tmp (cdr expr)))))))
         ((null (cdr b)) res))))

(defmacro ->> (&body body)
  "Evaluates the first expression of BODY, then inserts the result as
   the last argument of the second expression, evaluates that, and so on.
   The evaluation short-circuits as soon as an expression evaluates to NIL.

   For example:
   (->> some-hash-table
     (gethash \"metadata\")
     (gethash \"name\"))

   fetches the metadata key from some-hash-table, then fetches the name key
   from the metadata, and returns the result."
  (let ((tmp (gensym)))
    (do* ((b      body
                  (cdr b))
          (expr   (car b)
                  (car b))
          (res    expr
                  `(let ((,tmp ,res))
                     (when ,tmp
                       ,(cons (car expr)
                              (append (cdr expr) (list tmp)))))))
         ((null (cdr b)) res))))

(defmacro doto (&body body)
  "Evaluates the first expression of BODY, then inserts the result as
   the last argument of all the remaining expressions. Returns the
   result of the first expression.

   For example:
   (doto (make-hash-table)
     ((lambda (x) (setf (gethash \"hi\" x) 1)))
     ((lambda (x) (setf (gethash \"ho\" x) 2))))"
  (let ((expr (gensym)))
    `(let ((,expr ,(car body)))
       ,@ (mapcar (lambda (x) (concatenate 'list x (list expr))) (cdr body))
       ,expr)))

(defmacro _ (&body body)
  "Comment out the whole expression. Unfortunately returns a NIL value."
  (declare (ignore body)))

(defun filter (lst pred)
  "Filter a LST by a PREDicate."
  (nreverse
   (reduce
    (lambda (res el)
      (if (funcall pred el)
          (cons el res)
          res))
    lst
    :initial-value nil)))

(defun id (x)
  "The identity function."
  x)

(defmacro select (&body body)
  "Filter the first item of BODY (which is expected to be a list) and only keep
   items which satisfy the predicate (rest of the BODY). The BODY may contain
   expressions which walk the item down to the location that is to be used for the
   predicate.

   Example:
   (->. (kget \"Deployment\")
     (key \"items\")
     (select
       (key \"metadata\")
       (key \"name\")
       (predicate name (search \"farmer\"))))
   returns all deployments whose .metadata.name contains \"farmer\"."
  (let ((x (gensym)))
    `(filter ,(car body) (lambda (,x) (-> ,x ,@ (cdr body))))))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
               str))

(defun map-n (num fn lst)
  "Apply FN to NUM elements from LST and return the result."
  (labels ((f (n args l res)
             (cond ((null l)
                    (push (apply fn (nreverse args)) res)
                    (nreverse res))
                   ((= n num)
                    (push (apply fn (nreverse args)) res)
                    (f 1 (list (car l)) (cdr l) res))
                   (t
                    (f (1+ n) (cons (car l) args) (cdr l) res)))))
    (f 1 (list (car lst)) (cdr lst) nil)))

(defmacro ensure (val var &body body)
  "Set the variable VAR to VAL and evaluate the BODY. If the BODY evaluates
   to T, return VAL, otherwise signal a condition."
  `(if (let ((,var ,val)) ,@body)
       ,val
       (error (format nil "Guard ~a failed on ~a" (quote ,@body) ,val))))

(defun key (hashmap name)
  "Go \"down\" an object at key NAME and return that object.
   Create an empty one if it doesn't exist."
  (multiple-value-bind (ret existsp)
      (gethash name hashmap)
    (if existsp ret (setf (gethash name hashmap) (make-object)))))

(defun (setf key) (new-value hashmap name)
  (setf (gethash name hashmap) new-value))

(defun set-key (hashmap name value)
  "Set the key NAME in HASHMAP to VALUE and return the modified HASHMAP."
  (setf (key hashmap name) value)
  hashmap)

(defun del-key (hashmap name)
  (remhash name hashmap)
  hashmap)

(defun update-key (hashmap name f)
  (setf (key hashmap name) (funcall f (key hashmap name))))

(defun new-object (hashmap name)
  (set-key hashmap name (make-object)))

(defun new-list (hashmap name)
  (set-key hashmap name nil))

(defun add-to-list (hashmap name item)
  (update-key hashmap name
              #'(lambda (l)
                  (cons item l))))

(defun set-nth (list index value)
    (loop for i from 0
          for j in list
          collect (if (= i index) value j)))

(defmacro with (val var &body body)
  "Set the variable VAR to VAL and evaluate the BODY. Return VAR."
  `(let ((,var ,val))
     ,@body
     ,var))

(defun get-path (o path)
  "Follow PATH down the object O and return the subtree there. PATH is
   the list of keys to follow."
  (flet ((go-down-key-or-list-item (object index)
           (cond ((listp object) (nth index object))
                 ((hash-table-p object) (gethash index object))
                 (t (error (format nil "Can't traverse type ~a." (type-of object)))))))
    (reduce #'go-down-key-or-list-item path :initial-value o)))

(defun set-path (o path value)
  "Follow PATH down the object O and set the value to VALUE. PATH is the list of
   keys to follow. Return the modified object O."
  (let ((target (get-path o (butlast path)))
        (index  (car (last path))))
    (cond ((listp target) (set-nth target index value))
          ((hash-table-p target) (set-key target index value))
          (t (error (format nil "Can't traverse type ~a." (type-of target)))))
    o))

(defun deep-copy (o)
  "Return a deep copy of a data structure O which consists of (HASH-TABLE :TEST EQUALP), LIST, or some simple data type."
  (cond ((listp o)
         (nreverse (reduce #'(lambda (o it) (cons (deep-copy it) o)) o :initial-value nil)))
        ((hash-table-p o)
         (let ((new-obj (make-object)))
           (loop for k being the hash-keys of o using (hash-value val)
                 do (set-key new-obj k (deep-copy val)))
           new-obj))
        (t o)))

(defun diff (o1 o2 handler)
  "Compares two objects O1 and O2 and calls the HANDLER on any differences.
   HANDLER is a function which accepts a keyword (either :value-added,
   :value-changed, or :value-deleted), the key, the old value, and the new value."
  (maphash #'(lambda (k1 v1)
               (multiple-value-bind (v2 p2-p)
                   (gethash k1 o2)
                 (if p2-p
                     (unless (equalp v1 v2)
                       (funcall handler :value-changed k1 v1 v2))
                     (funcall handler :value-deleted k1 v1 nil))))
           o1)
  (maphash #'(lambda (k2 v2)
               (multiple-value-bind (v1 p1-p)
                   (gethash k2 o1)
                 (declare (ignore v1))
                 (unless p1-p
                   (funcall handler :value-added k2 nil v2))))
           o2))

(defmacro foreach (lst &body body)
  (let ((x (gensym)))
    `(mapcar (lambda (,x)
               (-> ,x ,@body))
             ,lst)))

(defun uniq (l &optional (equals-fn #'equalp))
  "Returns a new list, removing all the duplicates from l. l must be sorted."
  (reduce
   (lambda (el res)
     (if (funcall equals-fn (car res) el)
         res
         (cons el res)))
   l
   :initial-value nil
   :from-end t))

(defun safe-encode (string)
  "Encode strings so they can contain whitespace and special characters, but
   retain slashes and colons for better readability of path names."
  (-> (do-urlencode:urlencode string)
    ((lambda (x) (ppcre:regex-replace-all "%2F" x "/")))
    ((lambda (x) (ppcre:regex-replace-all "%3A" x ":")))))

(defun safe-decode (string)
  (do-urlencode:urldecode string))


;;-----------------------------------------------------------------------------
;; The following were all taken from Paul Graham's book "On Lisp"
;;-----------------------------------------------------------------------------

(proclaim '(inline last1 singlep append1 nconc1 mklist))

(defun last1 (lst)
  "Returns the last element of LST."
  (car (last lst)))

(defun singlep (lst)
  "Is LST a list containing just a single element?"
  (and (consp lst)
       (not (cdr lst))))

(defun append1 (lst obj)
  "Append OBJ to the list LST."
  (append lst (list obj)))

(defun nconc1 (lst obj)
  "Append OBJ to the list LST and mess up LST in the process."
  (nconc lst (list obj)))

(defun mklist (obj)
  "Return OBJ unchanged if it is already a list, otherwise wrap it in a new list."
  (if (listp obj)
      obj
      (list obj)))

(defun longer (x y)
  "Returns T if the sequence X is longer than Y."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun group (source n)
  "Group the list SOURCE into groups of N elements."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Return a list of all elements of X, removing the original structure."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  "Remove all elements in TREE which satisfy TEST, preserving the structure."
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

(defun find2 (fn lst)
  "Find the first occurrence in LST which satisfies FN. Return that element and the result of applying FN to it."
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Returns the sublist of LST starting at X if X occurs before Y, otherwise NIL."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Returns the sublist of LST starting at X if X comes after Y, otherwise NIL."
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Test if OBJ occurs more than once in LST and return the sublist of LST starting at the second occurrence."
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  "Split LST in two at the point where FN returns T for the first time."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  "Find the element in LST with the highest score according to FN."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max  (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))

(defun best (fn lst)
  "Find the element in LST which compares best according to the predicate FN."
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max    (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(defvar *!equivs* (make-hash-table :test #'equal))

(defmacro ! (fn)
  (let ((f (eval fn)))
    (or (gethash f *!equivs*) f)))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(def! #'remove-if #'delete-if)

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val hit) (gethash args cache)
        (if hit
            val
            (setf (gethash args cache)
                  (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  (lambda (x)
    (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

(defun fintersection (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fintersection fns)))
        (lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun funion (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'funion fns)))
        (lambda (x)
          (or (funcall fn x) (funcall chain x))))))

(defun run (program &rest args)
  "Call uiop:run-program but with outputs set to *standard-output* by default."
  (apply #'uiop:run-program
         program
         (append (list :output *standard-output*
                       :error-output *standard-output*)
                 args)))
