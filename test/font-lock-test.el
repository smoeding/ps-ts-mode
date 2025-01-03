;;; font-lock-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Stefan Möding

;; Author: Stefan Möding
;; Created: <2025-01-03 09:45:33 stm>
;; Updated: <2025-01-03 11:46:19 stm>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit test suite for ps-ts-mode

;;; Code:

(message "Running Emacs %s with tests from %s"
         emacs-version (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function ps-test-with-temp-buffer (content &rest body))
(declare-function ps-test-face-at (pos &optional content))


;;; Strings

(ert-deftest fontify/string ()
  (ps-test-with-temp-buffer "(foo)"
    (should (eq (ps-test-face-at 1) 'ps-ts-string))
    (should (eq (ps-test-face-at 2) 'ps-ts-string))))

(ert-deftest fontify/string-empty ()
  (ps-test-with-temp-buffer "()"
    (should (eq (ps-test-face-at 1) 'ps-ts-string))
    (should (eq (ps-test-face-at 2) 'ps-ts-string))))

(ert-deftest fontify/string-with-escaped-newline ()
  (ps-test-with-temp-buffer "(foo\\nbar)"
    (should (eq (ps-test-face-at 4) 'ps-ts-string))
    (should (eq (ps-test-face-at 5) 'ps-ts-escape))
    (should (eq (ps-test-face-at 6) 'ps-ts-escape))
    (should (eq (ps-test-face-at 7) 'ps-ts-string))))

(ert-deftest fontify/string-with-escaped-parenthesis ()
  (ps-test-with-temp-buffer "(foo\\)bar)"
    (should (eq (ps-test-face-at 4) 'ps-ts-string))
    (should (eq (ps-test-face-at 5) 'ps-ts-escape))
    (should (eq (ps-test-face-at 6) 'ps-ts-escape))
    (should (eq (ps-test-face-at 7) 'ps-ts-string))))

(ert-deftest fontify/string-with-balanced-parenthesis ()
  (ps-test-with-temp-buffer "(foo(baz)bar)"
   (should (eq (ps-test-face-at 3 ) 'ps-ts-string))
   (should (eq (ps-test-face-at 7) 'ps-ts-string))
   (should (eq (ps-test-face-at 11) 'ps-ts-string))))


;;; Numbers

(ert-deftest fontify/number-integer ()
  (ps-test-with-temp-buffer "42"
    (should (eq (ps-test-face-at 1) 'ps-ts-number))))

(ert-deftest fontify/number-float ()
  (ps-test-with-temp-buffer "4.2"
    (should (eq (ps-test-face-at 2) 'ps-ts-number))))

(ert-deftest fontify/number-scientific ()
  (ps-test-with-temp-buffer "4.2e12"
    (should (eq (ps-test-face-at 1) 'ps-ts-number))
    (should (eq (ps-test-face-at 2) 'ps-ts-number))
    (should (eq (ps-test-face-at 4) 'ps-ts-number))
    (should (eq (ps-test-face-at 5) 'ps-ts-number))))

(ert-deftest fontify/number-hex ()
  (ps-test-with-temp-buffer "16#42"
    (should (eq (ps-test-face-at 1) 'ps-ts-number))
    (should (eq (ps-test-face-at 3) 'ps-ts-number))
    (should (eq (ps-test-face-at 5) 'ps-ts-number))))

(ert-deftest fontify/number-radix ()
  (ps-test-with-temp-buffer "36#1z"
    (should (eq (ps-test-face-at 1) 'ps-ts-number))
    (should (eq (ps-test-face-at 3) 'ps-ts-number))
    (should (eq (ps-test-face-at 5) 'ps-ts-number))))

;;; Operators

(ert-deftest fontify/operator ()
  (ps-test-with-temp-buffer "3 4 add"
    (should (eq (ps-test-face-at 5) 'ps-ts-operator))))


;;; Comments

(ert-deftest fontify/comment ()
  (ps-test-with-temp-buffer "% comment"
    (should (eq (ps-test-face-at 1) 'ps-ts-comment))
    (should (eq (ps-test-face-at 3) 'ps-ts-comment))))

(ert-deftest fontify/comment-no-dsc ()
  (ps-test-with-temp-buffer "10 %% comment"
    (should (eq (ps-test-face-at 4) 'ps-ts-comment))
    (should (eq (ps-test-face-at 7) 'ps-ts-comment))))

(ert-deftest fontify/document-structure-comment ()
  (ps-test-with-temp-buffer "%!PS"
    (should (eq (ps-test-face-at 3) 'ps-ts-dsc))))


;;; Keywords

(ert-deftest fontify/keyword-def ()
  (ps-test-with-temp-buffer "/Foo 42 def"
    (should (eq (ps-test-face-at 10) 'ps-ts-keyword))))

;;; Literal

(ert-deftest fontify/literal-name ()
  (ps-test-with-temp-buffer "/Foo 42 def"
    (should (eq (ps-test-face-at 1) 'ps-ts-literal-name))
    (should (eq (ps-test-face-at 2) 'ps-ts-literal-name))))

;;; Constant

(ert-deftest fontify/constant-name ()
  (ps-test-with-temp-buffer "systemdict"
    (should (eq (ps-test-face-at 2) 'ps-ts-constant-name))))

;;; Font

(ert-deftest fontify/font-name ()
  (ps-test-with-temp-buffer "/Courier selectfont"
    (should (eq (ps-test-face-at 1) 'ps-ts-font-name))
    (should (eq (ps-test-face-at 2) 'ps-ts-font-name))))

;;; Compound objects

(ert-deftest fontify/dictionary ()
  (ps-test-with-temp-buffer "<< >>"
    (should (eq (ps-test-face-at 1) 'ps-ts-bracket))
    (should (eq (ps-test-face-at 4) 'ps-ts-bracket))))

(ert-deftest fontify/procedure ()
  (ps-test-with-temp-buffer "{ }"
    (should (eq (ps-test-face-at 1) 'ps-ts-bracket))
    (should (eq (ps-test-face-at 3) 'ps-ts-bracket))))

(ert-deftest fontify/array ()
  (ps-test-with-temp-buffer "[ ]"
    (should (eq (ps-test-face-at 1) 'ps-ts-bracket))
    (should (eq (ps-test-face-at 3) 'ps-ts-bracket))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-test.el ends here
