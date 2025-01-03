;;; test-helper.el --- unit-test setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

;;;; Load grammar

(require 'treesit)

;;;; Load ps-ts-mode

(let* ((cur (if load-in-progress load-file-name buffer-file-name))
       (dir (locate-dominating-file cur "Cask")))
  (load (expand-file-name "ps-ts-mode" dir)))

;;;; Utility functions

(defmacro ps-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ;; activate maximum decoration and disable doc-view
     (custom-set-variables '(treesit-font-lock-level 4)
                           '(ps-ts-inhibit-doc-view t))
     (ps-ts-mode)
     (font-lock-ensure)
     ,@body))

(defun ps-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current buffer."
  (if content
      (ps-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defconst ps-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.
Each symbol in this vector corresponding to the syntax code of
its index.")

(defun ps-test-syntax-at (pos)
  "Get the syntax class at POS.
Returns nil if there is no syntax class set at POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref ps-test-syntax-classes code)))

(defun ps-test-indent (code)
  "Test indentation of PostScript CODE.
The CODE argument is a string that should contain correctly
indented PostScript code.  The code is indented using
`indent-region' and the test succeeds if the result does not
differ from CODE."
  (ps-test-with-temp-buffer code
    (indent-region (point-min) (point-max))
    (should (string= (buffer-string) code))))

;;; test-helper.el ends here
