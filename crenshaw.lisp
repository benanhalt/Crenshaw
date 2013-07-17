;;;; crenshaw.lisp

(in-package #:crenshaw)

(defvar *look*)

(defvar *vars*)

(defun getchar ()
  (setf *look* (read-char)))

(defun expected (expected)
  (error (concatenate 'string "Expected: '" expected "'")))

(defun match (x)
  (if (eq *look* x) (getchar) (expected x)))

(defun skip-newline ()
  (if (eq *look* #\newline) (getchar)))

(defun addop-p (x)
  (find x '(#\+ #\-)))

(defun getname ()
  (if (alpha-char-p *look*)
      (prog1
          (string-upcase *look*)
        (getchar))
      (expected "Name")))

(defun getnum ()
  (if (not (digit-char-p *look*)) (expected "Integer"))
  (parse-integer
   (coerce
    (loop while (digit-char-p *look*)
       collect (prog1 *look* (getchar)))
    'string)))

(defun init ()
  (setf *vars* (make-hash-table :test 'equal))
  (getchar))

(defun factor ()
  (cond
    ((eq #\( *look*)
     (prog2
         (match #\()
         (expression)
       (match #\))))
    ((alpha-char-p *look*) (gethash (getname) *vars* 0))
    (t (getnum))))

(defun term ()
  (let ((value (factor)))
    (loop while (find *look* '(#\* #\/)) do
         (case *look*
           (#\*
            (match #\*)
            (setf value (* value (factor))))
           (#\/
            (match #\/)
            (setf value (/ value (factor))))))
    value))

(defun expression ()
  (let ((value (if (addop-p *look*) 0 (term))))
    (loop while (addop-p *look*) do
         (case *look*
           (#\+
            (match #\+)
            (setf value (+ value (term))))
           (#\-
            (match #\-)
            (setf value (- value (term))))))
    value))

(defun assignment ()
  (let ((name (getname)))
    (match #\=)
    (let ((value (expression)))
      (setf (gethash name *vars*) value)
      value)))

(defun main ()
  (init)
  (loop do
       (format t "    ~a~&" (assignment))
       (skip-newline)
     while (not (eq *look* #\.)))

  (loop for v being the hash-keys in *vars* do
       (format t "~a = ~a~&" v (gethash v *vars*)))
  nil)



