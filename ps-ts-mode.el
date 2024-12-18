;;; ps-ts-mode.el --- Major mode for PostScript using Tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024  Stefan Möding

;; Author:           Stefan Möding <stm@kill-9.net>
;; Maintainer:       Stefan Möding <stm@kill-9.net>
;; Version:          0.1.0
;; Created:          <2024-12-16 20:28:08 stm>
;; Updated:          <2024-12-18 13:21:04 stm>
;; URL:              https://github.com/smoeding/ps-ts-mode
;; Keywords:         languages
;; Package-Requires: ((emacs "29.1"))

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

;; This package uses a Tree-sitter parser to provide syntax highlighting,
;; indentation and navigation for the PostScript page description language.
;; PostScript is a tradmark of Adobe Inc.
;;
;; The package uses a Tree-sitter library to parse PostScript code and you
;; need to install the appropriate parser.  It can be done by using this
;; Elisp code:
;;
;;    (require 'ps-ts-mode)
;;    (ps-ts-mode-install-grammar)
;;
;; Note that a C compiler is required for this step.  Using the function
;; provided by the package ensures that a version of the parser matching the
;; package will be installed.  These commands should also be used to update
;; the parser to the correct version when the package is updated.

;;; Code:


;; Requirements

(require 'treesit)


;; Customization

(defgroup ps-ts nil
  "Create PostScript in Emacs."
  :prefix "ps-ts-"
  :group 'languages
  :link '(url-link :tag "Repository" "https://github.com/smoeding/ps-ts-mode"))


;; Faces

(defface ps-ts-comment
  '((t :inherit font-lock-comment-face))
  "Face for PostScript comments."
  :group 'ps-ts)

(defface ps-ts-dsc
  '((t :inherit font-lock-preprocessor-face))
  "Face for PostScript document structure comments."
  :group 'ps-ts)

(defface ps-ts-string
  '((t :inherit font-lock-string-face))
  "Face for PostScript strings."
  :group 'ps-ts)

(defface ps-ts-escape
  '((t :inherit font-lock-escape-face))
  "Face for escape sequences."
  :group 'ps-ts)

(defface ps-ts-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for PostScript keywords."
  :group 'ps-ts)

(defface ps-ts-operator
  '((t :inherit font-lock-type-face))
  "Face for common PostScript operators."
  :group 'ps-ts)

(defface ps-ts-constant-name
  '((t :inherit font-lock-constant-face))
  "Face for PostScript constant names."
  :group 'ps-ts)

(defface ps-ts-literal-name
  '((t :inherit font-lock-variable-name-face))
  "Face for PostScript literal names."
  :group 'ps-ts)

(defface ps-ts-font-name
  '((t :inherit font-lock-function-name-face))
  "Face for the name of a builtin PostScript font."
  :group 'ps-ts)

(defface ps-ts-number
  '((t :inherit font-lock-number-face))
  "Face for numbers."
  :group 'ps-ts)

(defface ps-ts-bracket
  '((t :inherit font-lock-bracket-face))
  "Face for brackets."
  :group 'ps-ts)

(defface ps-ts-warning
  '((t :inherit font-lock-warning-face))
  "Face for language errors found by the parser."
  :group 'ps-ts)


;; Language grammar

(defconst ps-ts-mode-treesit-language-source
  '(postscript . ("https://github.com/smoeding/tree-sitter-postscript"))
  "The language source entry for the associated PostScript parser.
The value refers to the specific version of the parser that the
mode has been tested with.  Using this mode with either an older
or newer version of the parser might not work as expected.")

(defun ps-ts-mode-install-grammar ()
  "Install the language grammar for `ps-ts-mode'.
The function removes existing entries for the PostScript language
in `treesit-language-source-alist' and adds the entry stored in
`ps-ts-mode-treesit-language-source'."
  (interactive)
  ;; Remove existing entries
  (setq treesit-language-source-alist
        (assq-delete-all 'postscript treesit-language-source-alist))
  ;; Add the correct entry
  (add-to-list 'treesit-language-source-alist
               ps-ts-mode-treesit-language-source)
  ;; Install the grammar
  (treesit-install-language-grammar 'postscript))


;; Font-Lock

(defvar ps-ts--operators-regex
  (regexp-opt '("forall" "dict" "begin" "end" "def" "exec" "if" "ifelse"
                "for" "repeat" "loop" "exit" "stop" "stopped"
                "countexecstack" "execstack" "quit" "start" "save" "restore"
                "bind" "gsave" "grestore" "grestoreall" "showpage")
              'symbols)
  "Regular expression matching important operator names.")

(defvar ps-ts--builtin-regex
  (regexp-opt '("add" "div" "idiv" "mod" "mul" "sub" "abs" "neg" "ceiling"
                "floor" "round" "truncate" "sqrt" "atan" "cos" "sin" "exp"
                "ln" "log" "rand" "srand" "rrand"
                ;; Stack manipulation operators
                "pop" "exch" "dup" "copy" "index" "roll" "clear" "count"
                "mark" "counttomark")
              'symbols)
  "Regular expression matching builtin operator names.")

(defvar ps-ts--constants-regex
  (regexp-opt '("null" "true" "false"
                ;; Standard local dictionaries
                "$error" "errordict" "statusdict" "userdict" "FontDirectory"
                ;; Standard global dictionaries
                "globaldict" "systemdict" "GlobalFontDirectory"
                ;; User objects
                "UserObjects"
                ;; Job execution environment
                "serverdict"
                ;; Default encodings
                "StandardEncoding" "ISOLatin1Encoding")
              'symbols)
  "Regular expression matching all constant names.")

(defvar ps-ts--fonts-regex
  (regexp-opt '("/AvantGarde-Book"
                "/AvantGarde-BookOblique"
                "/AvantGarde-Demi"
                "/AvantGarde-DemiOblique"
                "/Bookman-Demi"
                "/Bookman-DemiItalic"
                "/Bookman-Light"
                "/Bookman-LightItalic"
                "/Courier"
                "/Courier-Bold"
                "/Courier-BoldOblique"
                "/Courier-Oblique"
                "/Helvetica"
                "/Helvetica-Bold"
                "/Helvetica-BoldOblique"
                "/Helvetica-Narrow"
                "/Helvetica-Narrow-Bold"
                "/Helvetica-Narrow-BoldOblique"
                "/Helvetica-Narrow-Oblique"
                "/Helvetica-Oblique"
                "/NewCenturySchlbk-Bold"
                "/NewCenturySchlbk-BoldItalic"
                "/NewCenturySchlbk-Italic"
                "/NewCenturySchlbk-Roman"
                "/Palatino-Bold"
                "/Palatino-BoldItalic"
                "/Palatino-Italic"
                "/Palatino-Roman"
                "/Symbol"
                "/Times-Bold"
                "/Times-BoldItalic"
                "/Times-Italic"
                "/Times-Roman"
                "/ZapfChancery-MediumItalic"
                "/ZapfDingbats")
              'symbols)
  "Regular expression matching the 35 standard builtin fonts.")

(defvar ps-ts-mode-feature-list
  ;; Level 1 usually contains only comments and definitions.
  ;; Level 2 usually adds keywords, strings, data types, etc.
  ;; Level 3 usually represents full-blown fontifications, including
  ;; assignments, constants, numbers and literals, etc.
  ;; Level 4 adds everything else that can be fontified: delimiters,
  ;; operators, brackets, punctuation, all functions, properties,
  ;; variables, etc.
  '((comment)
    (keyword builtin string literal)
    (constant number escape-sequence)
    (error brackets))
  "`treesit-font-lock-feature-list' for `ps-ts-mode'.")

(defvar ps-ts-mode-font-lock-settings
  `( ;;
    :feature comment
    :language postscript
    (((comment) @ps-ts-comment)
     ((document_structure_comment) @ps-ts-dsc))

    :feature string
    :language postscript
    (((string) @ps-ts-string))

    :feature escape-sequence
    :language postscript
    :override t
    ((string (escape_sequence) @ps-ts-escape))

    :feature number
    :language postscript
    ((numeric) @ps-ts-number)

    :feature literal
    :language postscript
    (((literal) @ps-ts-font-name (:match ,ps-ts--fonts-regex @ps-ts-font-name))
     ((literal) @ps-ts-literal-name))

    :feature constant
    :language postscript
    (((operator) @ps-ts-constant-name
      (:match ,ps-ts--constants-regex @ps-ts-constant-name)))

    :feature keyword
    :language postscript
    (((operator) @ps-ts-keyword
      (:match ,ps-ts--operators-regex @ps-ts-keyword)))

    :feature builtin
    :language postscript
    (((operator ) @ps-ts-operator
      (:match ,ps-ts--builtin-regex @ps-ts-operator)))

    :feature brackets
    :language postscript
    (((dictionary ["<<" ">>"] @ps-ts-bracket))
     ((procedure ["{" "}"] @ps-ts-bracket))
     ((array ["[" "]"] @ps-ts-bracket)))

    :feature error
    :language postscript
    :override t
    ((ERROR) @ps-ts-warning))
  "`treesit-font-lock-settings' for `ps-ts-mode'.")


;; Indentation

(defcustom ps-ts-indent-level 2
  "Number of spaces for each indententation step."
  :group 'ps-ts
  :type 'integer
  :safe 'integerp)

(defcustom ps-ts-indent-tabs-mode nil
  "Indentation can insert tabs in PostScript mode if this is non-nil."
  :group 'ps-ts
  :type 'boolean
  :safe 'booleanp)

(defvar ps-ts-indent-rules
  `((postscript
     ;; top-level elements start in column zero
     ((parent-is "document") column-0 0)
     ;; composite objects
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ">>") parent-bol 0)
     ((parent-is "array") parent-bol ps-ts-indent-level)
     ((parent-is "procedure") parent-bol ps-ts-indent-level)
     ((parent-is "dictionary") parent-bol ps-ts-indent-level)
     ;; default
     (catch-all parent-bol 0)))
  "Indentation rules for `ps-ts-mode'.")


;; Major mode definition

(defvar ps-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Line comments
    (modify-syntax-entry ?\% "< " table)
    (modify-syntax-entry ?\n "> " table)
    (modify-syntax-entry ?\r "> " table)
    (modify-syntax-entry ?\f "> " table)
    ;; Strings
    (modify-syntax-entry ?\< "(>" table)
    (modify-syntax-entry ?\> ")<" table)
    ;; Symbols
    (modify-syntax-entry ?\! "_ " table)
    (modify-syntax-entry ?\" "_ " table)
    (modify-syntax-entry ?\# "_ " table)
    (modify-syntax-entry ?\$ "_ " table)
    (modify-syntax-entry ?\& "_ " table)
    (modify-syntax-entry ?\' "_ " table)
    (modify-syntax-entry ?\* "_ " table)
    (modify-syntax-entry ?\+ "_ " table)
    (modify-syntax-entry ?\, "_ " table)
    (modify-syntax-entry ?\- "_ " table)
    (modify-syntax-entry ?\. "_ " table)
    (modify-syntax-entry ?\: "_ " table)
    (modify-syntax-entry ?\; "_ " table)
    (modify-syntax-entry ?\= "_ " table)
    (modify-syntax-entry ?\? "_ " table)
    (modify-syntax-entry ?\@ "_ " table)
    (modify-syntax-entry ?^  "_ " table)
    (modify-syntax-entry ?\_ "_ " table)
    (modify-syntax-entry ?\` "_ " table)
    (modify-syntax-entry ?\| "_ " table)
    (modify-syntax-entry ?\~ "_ " table)
    ;; The backslash is our escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `ps-ts-mode' buffers.")

;;;###autoload
(define-derived-mode ps-ts-mode prog-mode "PostScript"
  "Major mode for editing PostScript files, powered by tree-sitter.
\\<ps-ts-mode-map>
Syntax highlighting for standard PostScript elements (operators,
comments, strings, keywords) is available.  You can customize the
variable `treesit-font-lock-level' to control the level of
fontification.

Indentation is implemented for array, procedure and dictionary
element.  Customize `ps-ts-indent-level' to set the level of
indentation to use.

The mode needs the Tree-sitter parser for PostScript code.
A parser suitable for the current package version can be
installed using the function `ps-ts-mode-install-grammar'.  Some
development tools (C compiler, ...) are required for this.

Indentation and fontification depend on the concrete syntax tree
returned by the Tree-sitter parser.  So errors like a missing
closing parenthesis or bracket can lead to wrong indentation or
missing fontification.  This is easily resolved by fixing the
particular syntax error.

\\{ps-ts-mode-map}"
  (setq-local require-final-newline mode-require-final-newline)

  ;; Comments
  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local comment-start-skip "%+[ \t]*")

  ;; DocView minor mode allows to view the current PostScript file
  (doc-view-minor-mode 1)

  ;; Treesitter
  (when (treesit-ready-p 'postscript)
    (treesit-parser-create 'postscript)

    ;; Navigation
    (setq treesit-defun-type-regexp "\\`document_structure_comment\\'")

    ;; Font-Lock
    (setq treesit-font-lock-feature-list ps-ts-mode-feature-list)
    (setq treesit-font-lock-settings (apply #'treesit-font-lock-rules
                                            ps-ts-mode-font-lock-settings))

    ;; Indentation
    (setq indent-tabs-mode ps-ts-indent-tabs-mode)
    (setq treesit-simple-indent-rules ps-ts-indent-rules)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.e?ps\\'" . ps-ts-mode))

;;;###autoload
(add-to-list 'magic-mode-alist '("^%!PS\\>" . ps-ts-mode))

(provide 'ps-ts-mode)

;;; ps-ts-mode.el ends here
