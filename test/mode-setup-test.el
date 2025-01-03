;;; mode-setup-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Stefan Möding

;; Author: Stefan Möding
;; Created: <2025-01-03 09:05:06 stm>
;; Updated: <2025-01-03 09:54:55 stm>

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
(require 'treesit)


;;; Tests
(ert-deftest postscript/treesitter-loaded ()
  :tags '(library)
  (should (featurep 'treesit)))

(ert-deftest postscript/treesitter-parser-loaded ()
  :tags '(library)
  (should (treesit-ready-p 'postscript)))

(ert-deftest postscript/ps-ts-mode-loaded ()
  :tags '(library)
  (should (featurep 'ps-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-setup-test.el ends here
