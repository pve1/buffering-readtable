(defpackage #:buffering-readtable
  (:use #:cl)
  (:export #:make-buffering-readtable))

(in-package :buffering-readtable)

;;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ac.htm
;;; #\Tab is not STANDARD-CHAR for some reason.

(defvar *standard-characters*
  '(#\Newline #\Space
    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
    #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
    #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~))

(defun read-all-from-stream (stream)
  (loop :for form = (read stream nil stream nil)
        :until (eq form stream)
        :collect form))

(defun read-all-from-string (string)
  (with-input-from-string (string-stream string)
    (read-all-from-stream string-stream)))

(defun make-buffering-readtable (&key (translate-all #'identity)
                                      (translate-one #'identity)
                                      (translate-string #'identity)
                                      (characters *standard-characters*)
                                      (inner-readtable *readtable*))
"Creates a readtable that buffers all input from a non-interactive
stream passed to READ. The functions TRANSLATE-ALL, TRANSLATE-ONE and
TRANSLATE-STRING are used to transform the buffered input. Calling
READ will then return the transformed input instead of the actual input.

When converting the input string into a list of forms, *READTABLE* is
bound to INNER-READTABLE.

Translations will be applied in the following order:

  TRANSLATE-STRING will be applied to the string received from the stream.

  TRANSLATE-ONE will be applied to each top-level form.

  TRANSLATE-ALL will be applied to a list of all forms received from the stream.

Example:

  (let ((*readtable* (make-buffering-readtable
                      :translate-all (lambda (forms)
                                       (append forms (list 'extra))))))
    (with-input-from-string (s \"a b c\")
      (list (read s)
            (read s)
            (read s)
            (read s))))

=> (A B C EXTRA)
"
  (let* ((rt (copy-readtable nil))
         (character-buffer (make-array 256 :adjustable t :fill-pointer 0))
         (form-buffer ())
         (form-buffer-initialized-p nil)
         (reader-macro-function
           ;; Note: This function will never get called if the stream
           ;; is empty, so at least one character is needed.
           (lambda (s c)
             (if (listen s) ; Not at eof, i.e. C is not the last character.
                 (progn
                   ;; Collect character into character-buffer and
                   ;; return (values), which instructs the reader to
                   ;; move on to the next character without receiving
                   ;; a value.
                   (vector-push-extend c
                                       character-buffer
                                       (* 2 (array-total-size character-buffer)))
                   (values))
                 ;; Last character
                 (progn
                   ;; Initialize form buffer from character buffer,
                   ;; which should at this point contain everything
                   ;; except the last character.
                   (unless form-buffer-initialized-p
                     ;; Add the last character.
                     (vector-push-extend c
                                         character-buffer
                                         (* 2 (array-total-size character-buffer)))
                     ;; Read everything from character-buffer into
                     ;; form-buffer using the inner readtable,
                     ;; applying TRANSLATE-STRING, TRANSLATE-ONE and
                     ;; TRANSLATE-ALL.
                     (let ((*readtable* inner-readtable))
                       (setf form-buffer
                             (funcall translate-all
                                      (mapcar translate-one
                                              (read-all-from-string
                                               (funcall translate-string
                                                        (coerce character-buffer 'string)))))
                             form-buffer-initialized-p t)))

                   (if (not (null form-buffer))
                       (progn
                         ;; Repeatedly bounce the last character back
                         ;; until form-buffer is empty.
                         (unread-char c s)
                         ;; Return the next form from form-buffer as
                         ;; if it had just been read
                         (pop form-buffer))
                       (values)))))))   ; Reader will deal with eof.
    (dolist (c characters)
      (set-macro-character c
                           reader-macro-function
                           nil
                           rt))
    rt))
