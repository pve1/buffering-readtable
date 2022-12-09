;; Load this file

(require :buffering-readtable)

(in-package :buffering-readtable)

(defmacro run-tests (&rest tests)
  `(progn
    ,@(loop :for test :in tests
            :collect `(assert ,test))
    t))

(run-tests
 ;; No translation.
 (equal '(a b c) (let ((*readtable* (make-buffering-readtable)))
                   (with-input-from-string (s "a b c")
                     (list (read s)
                           (read s)
                           (read s)))))
 (equal '(a (b (c))) (let ((*readtable* (make-buffering-readtable)))
                       (with-input-from-string (s "a (b (c))")
                         (list (read s)
                               (read s)))))
 ;; Translate one
 (equal '(a b c) (let ((*readtable* (make-buffering-readtable
                                     :translate-one (lambda (x)
                                                      (if (eq x 'x)
                                                          'c
                                                          x)))))
                   (with-input-from-string (s "a b x")
                     (list (read s)
                           (read s)
                           (read s)))))
 ;; Translate all
 (equal '(a b c) (let ((*readtable* (make-buffering-readtable
                                     :translate-all (lambda (x)
                                                      (if (equal x '(x))
                                                          '(a b c)
                                                          x)))))
                   (with-input-from-string (s "x")
                     (list (read s)
                           (read s)
                           (read s)))))
 ;; Append using translate-all.
 (equal '(a b c extra) (let ((*readtable* (make-buffering-readtable
                                           :translate-all (lambda (forms)
                                                            (append forms (list 'extra))))))
                         (with-input-from-string (s "a b c")
                           (list (read s)
                                 (read s)
                                 (read s)
                                 (read s)))))
 (typep (handler-case (let ((*readtable* (make-buffering-readtable)))
                        (with-input-from-string (s "(")
                          (read s)))
          (error (c) c))
        'end-of-file)
 (typep (handler-case (let ((*readtable* (make-buffering-readtable)))
                        (with-input-from-string (s "")
                          (read s)))
          (error (c) c))
        'end-of-file))

(format t "All ok.~%")
