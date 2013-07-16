;;;; crenshaw.lisp

(in-package #:crenshaw)

(defvar *look*)

(defun getchar ()
  (setf *look* (read-char)))

(defun expected (expected)
  (error (concatenate 'string "Expected: '" expected "'")))

(defun match (x)
  (if (equal *look* x) (getchar) (expected x)))

(defun getname ()
  (if (alpha-char-p *look*)
      (prog1
          (string-upcase *look*)
        (getchar))
      (expected "Name")))

(defun getnum ()
  (if (digit-char-p *look*)
      (prog1 *look* (getchar))
      (expected "Integer")))

(defun emit (s)
  (format t "    ~a" s))

(defun emitln (s)
  (format t "    ~a~&" s))

(defun init ()
  (getchar))

(defun expression ()
  (emitln (concatenate 'string "MOVE #" (string (getnum)) ", D0")))

(defun main ()
  (init)
  (expression))



