;;; eldoc-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Stefan Möding

;; Author: Stefan Möding
;; Created: <2025-01-03 09:45:33 stm>
;; Updated: <2025-07-29 18:48:27 stm>

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
(declare-function ps-ts-mode-eldoc-operator (&rest _unused))



(ert-deftest eldoc/showpage ()
  (ps-test-with-temp-buffer "showpage"
    (beginning-of-line)
    (should (equal (ps-ts-mode-eldoc-operator) "-  showpage  -"))))

(ert-deftest eldoc/transform ()
  (ps-test-with-temp-buffer "10 20 transform"
    (end-of-line)
    (should (equal (ps-ts-mode-eldoc-operator) "x y  transform  x' y'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eldoc-test.el ends here
