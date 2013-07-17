;;;; crenshaw.lisp

(in-package #:crenshaw)

(defvar *look*)

(defun getchar ()
  (setf *look* (read-char)))

(defun expected (expected)
  (error (concatenate 'string "Expected: '" expected "'")))

(defun match (x)
  (cond
    ((eq *look* x)
     (getchar)
     (skip-whitespace))
    (t (expected x))))

(defun addop-p (x)
  (find x '(#\+ #\-)))

(defun getname ()
  (prog2
      (if (not (alpha-char-p *look*))
          (expected "Name"))

      (string-upcase
       (coerce
        (loop while (alphanumericp *look*)
           collecting (prog1 *look* (getchar)))
        'string))

    (skip-whitespace)))

(defun getnum ()
  (prog2
      (if (not (digit-char-p *look*))
          (expected "Integer"))

      (coerce
       (loop while (digit-char-p *look*)
          collecting (prog1 *look* (getchar)))
       'string))

  (skip-whitespace))

(defun emit (&rest items)
  (format t "    ~{~a~}" items))

(defun emitln (&rest items)
  (apply #'emit items)
  (format t "~&"))

(defun skip-whitespace ()
  (loop while (eq #\space *look*)
       do (getchar)))

(defun init ()
  (getchar)
  (skip-whitespace))

(defun ident ()
  (let ((name (getname)))
    (cond
      ((eq #\( *look*)
       (match #\()
       (match #\))
       (emitln "BSR " name))
      (t (emitln "MOVE " name "(PC), D0")))))

(defun factor ()
  (cond
    ((eq #\( *look*)
     (match #\()
     (expression)
     (match #\)))
    ((alpha-char-p *look*) (ident))
    (t (emitln "MOVE #" (getnum) ", D0"))))

(defun term ()
  (factor)
  (loop while (find *look* '(#\* #\/)) do
       (emitln "MOVE D0, -(SP)")
       (case *look*
         (#\* (multiply))
         (#\/ (divide)))))

(defun expression ()
  (if (addop-p *look*)
      (emitln "CLR D0")
      (term))
  (loop while (addop-p *look*) do
       (emitln "MOVE D0, -(SP)")
       (case *look*
         (#\+ (add))
         (#\- (subtract)))))

(defun assignment ()
  (let ((name (getname)))
    (match #\=)
    (expression)
    (emitln "LEA " name "(PC), A0")
    (emitln "MOV D0, A0")))

(defun add ()
  (match #\+)
  (term)
  (emitln "ADD (SP)+, D0"))

(defun subtract ()
  (match #\-)
  (term)
  (emitln "SUB (SP)+, D0")
  (emitln "NEG D0"))

(defun multiply ()
  (match #\*)
  (factor)
  (emitln "MULS (SP)+, D0"))

(defun divide ()
  (match #\/)
  (factor)
  (emitln "MOVE (SP)+, D1")
  (emitln "DIVS D1, D0"))

(defun main ()
  (init)
  (assignment)
  (if (not (eq *look* #\newline))
      (expected "newline")))



