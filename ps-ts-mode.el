;;; ps-ts-mode.el --- Major mode for PostScript using Tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024, 2025  Stefan Möding

;; Author:           Stefan Möding <stm@kill-9.net>
;; Maintainer:       Stefan Möding <stm@kill-9.net>
;; Version:          0.1.0
;; Created:          <2024-12-16 20:28:08 stm>
;; Updated:          <2025-07-30 11:56:07 stm>
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

(defcustom ps-ts-inhibit-doc-view nil
  "Non-nil prevents automatic enabling of `doc-view-minor-mode'."
  :group 'ps-ts
  :type 'boolean
  :safe 'booleanp)

(defcustom ps-ts-important-operators
  '("begin" "bind" "countexecstack" "def" "dict" "end" "exec" "execstack"
    "exit" "for" "forall" "grestore" "grestoreall" "gsave" "if" "ifelse"
    "loop" "quit" "repeat" "restore" "save" "showpage" "start" "stop"
    "stopped")
  "Important operators that should be displayed in a different face."
  :group 'ps-ts
  :type '(list string))

(defcustom ps-ts-special-operators
  '("null" "true" "false"
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
  "Special language features and operators shown in a different face."
  :group 'ps-ts
  :type '(list string))

(defcustom ps-ts-font-names
  '("/AvantGarde-Book"
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
  "PostScript font names."
  :group 'ps-ts
  :type '(list string))


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
  '(postscript . ("https://github.com/smoeding/tree-sitter-postscript"
                  "v1.1.0"))
  "The language source entry for the associated PostScript parser.
The value refers to the specific version of the parser that the mode has
been tested with.  Using this mode with either an older or newer version
of the parser might not work as expected.")

(defun ps-ts-mode-install-grammar ()
  "Install the language grammar for `ps-ts-mode'.
The function removes existing entries for the PostScript language in
`treesit-language-source-alist' and adds the entry stored in
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


;; Eldoc

(defvar ps-ts-operator-summary-alist
  '(
    ;;
    ;; Operand Stack Manipulation Operators
    ;;
    ("pop"
     "Discard top element"
     "any" "-")
    ("exch"
     "Exchange top two elements"
     "any1 any2" "any2 any1")
    ("dup"
     "Duplicate top element"
     "any" "any any")
    ("copy"
     "Duplicate top n elements"
     "any1 ... anyn n" "any1 ... anyn any1 ... anyn")
    ("index"
     "Duplicate arbitrary element"
     "anyn ... any0 n" "anyn ... any0 anyn")
    ("roll"
     "Roll n elements up j times"
     "anyn-1 ... any0 n j" "any(j-1) mod n ... any0 anyn-1 ... anyj mod n")
    ("clear"
     "Discard all elements"
     "|- any1 ... anyn" "|-")
    ("count"
     "Count elements on stack"
     "|- any1 ... anyn" "|- any1 ... anyn n")
    ("mark"
     "Push mark on stack"
     "-" "mark")
    ("cleartomark"
     "Discard elements down through mark"
     "mark obj1 ... objn" "-")
    ("counttomark"
     "Count elements down to mark"
     "mark obj1 ... objn" "mark obj1 ... objn n")
    ;;
    ;; Arithmetic and Math Operators
    ;;
    ("add"
     "Return num1 plus num2"
     "num1 num2" "sum")
    ("div"
     "Return num1 divided by num2"
     "num1 num2" "quotient")
    ("idiv"
     "Return int1 divided by int2"
     "int1 int2" "quotient")
    ("mod"
     "Return remainder after dividing int1 by int2"
     "int1 int2" "remainder")
    ("mul"
     "Return num1 times num2"
     "num1 num2" "product")
    ("sub"
     "Return num1 minus num2"
     "num1 num2" "difference")
    ("abs"
     "Return absolute value of num1"
     "num1" "num2")
    ("neg"
     "Return negative of num1"
     "num1" "num2")
    ("ceiling"
     "Return ceiling of num1"
     "num1" "num2")
    ("floor"
     "Return floor of num1"
     "num1" "num2")
    ("round"
     "Round num1 to nearest integer"
     "num1" "num2")
    ("truncate"
     "Remove fractional part of num1"
     "num1" "num2")
    ("sqrt"
     "Return square root of num"
     "num" "real")
    ("atan"
     "Return arctangent of num/den in degrees"
     "num den" "angle")
    ("cos"
     "Return cosine of angle degrees"
     "angle" "real")
    ("sin"
     "Return sine of angle degrees"
     "angle" "real")
    ("exp"
     "Raise base to exponent power"
     "base exponent" "real")
    ("ln"
     "Return natural logarithm (base e)"
     "num" "real")
    ("log"
     "Return common logarithm (base 10)"
     "num" "real")
    ("rand"
     "Generate pseudo-random integer"
     "-" "int")
    ("srand"
     "Set random number seed"
     "int" "-")
    ("rrand"
     "Return random number seed"
     "-" "int")
    ;;
    ;; Array Operators
    ;;
    ("array"
     "Create array of length int"
     "int" "array")
    ("["
     "Start array construction"
     "-" "mark")
    ("]"
     "End array construction"
     "mark obj0 ... objn-1" "array")
    ("length"
     "Return number of elements in array"
     "array" "int")
    ("get"
     "Return array element indexed by index"
     "array index" "any")
    ("put"
     "Put any into array at index"
     "array index any" "-")
    ("getinterval"
     "Return subarray of array starting at index for count elements"
     "array index count" "subarray")
    ("putinterval"
     "Replace subarray of array1 starting at index by array2 | packedarray2"
     "array1 index array2 | packedarray2" "-")
    ("astore"
     "Pop elements from stack into array"
     "any0 ... anyn-1 array" "array")
    ("aload"
     "Push all elements of array on stack"
     "array" "any0 ... anyn-1 array")
    ("copy"
     "Copy elements of array1 to initial subarray of array2"
     "array1 array2" "subarray2")
    ("forall"
     "Execute proc for each element of array"
     "array proc" "-")
    ;;
    ;; Packed Array Operators
    ;;
    ("packedarray"
     "Create packed array consisting of n elements from stack"
     "any0 ... anyn-1 n" "packedarray")
    ("setpacking"
     "Set array packing mode for { ... } syntax (true=packed array)"
     "bool" "-")
    ("currentpacking"
     "Return array packing mode"
     "-" "bool")
    ("length"
     "Return number of elements in packedarray"
     "packedarray" "int")
    ("get"
     "Return packedarray element indexed by index"
     "packedarray index" "any")
    ("getinterval"
     "Return subarray of packedarray starting at index for count elements"
     "packedarray index count" "subarray")
    ("aload"
     "Push all elements of packedarray on stack"
     "packedarray" "any0 ... anyn-1 packedarray")
    ("copy"
     "Copy elements of packedarray1 to initial subarray of array2"
     "packedarray1 array2" "subarray2")
    ("forall"
     "Execute proc for each element of packedarray"
     "packedarray proc" "-")
    ;;
    ;; Dictionary Operators
    ;;
    ("dict"
     "Create dictionary with capacity for int elements"
     "int" "dict")
    ("<<"
     "Start dictionary construction"
     "-" "mark")
    (">>"
     "End dictionary construction"
     "mark key1 value1 ... keyn valuen" "dict")
    ("length"
     "Return number of entries in dict"
     "dict" "int")
    ("maxlength"
     "Return current capacity of dict"
     "dict" "int")
    ("begin"
     "Push dict on dictionary stack"
     "dict" "-")
    ("end"
     "Pop current dictionary off dictionary stack"
     "-" "-")
    ("def"
     "Associate key and value in current dictionary"
     "key value" "-")
    ("load"
     "Search dictionary stack for key and return associated value"
     "key" "value")
    ("store"
     "Replace topmost definition of key"
     "key value" "-")
    ("get"
     "Return value associated with key in dict"
     "dict key" "any")
    ("put"
     "Associate key with value in dict"
     "dict key value" "-")
    ("undef"
     "Remove key and its value from dict"
     "dict key" "-")
    ("known"
     "Test whether key is in dict"
     "dict key" "bool")
    ("where"
     "Find dictionary in which key is defined or false"
     "key" "dict true")
    ("copy"
     "Copy contents of dict1 to dict2"
     "dict1 dict2" "dict2")
    ("forall"
     "Execute proc for each entry in dict"
     "dict proc" "-")
    ("currentdict"
     "Return current dictionary"
     "-" "dict")
    ("errordict"
     "Return error handler dictionary"
     "-" "dict")
    ("$error"
     "Return error control and status dictionary"
     "-" "dict")
    ("systemdict"
     "Return system dictionary"
     "-" "dict")
    ("userdict"
     "Return writeable dictionary in local VM"
     "-" "dict")
    ("globaldict"
     "Return writeable dictionary in global VM"
     "-" "dict")
    ("statusdict"
     "Return product-dependent dictionary"
     "-" "dict")
    ("countdictstack"
     "Count elements on dictionary stack"
     "-" "int")
    ("dictstack"
     "Copy dictionary stack into array"
     "array" "subarray")
    ("cleardictstack"
     "Pop all nonpermanent dictionaries off dictionary stack"
     "-" "-")
    ;;
    ;; String Operators
    ;;
    ("string"
     "Create string of length int"
     "int" "string")
    ("length"
     "Return number of elements in string"
     "string" "int")
    ("get"
     "Return string element indexed by index"
     "string index" "int")
    ("put"
     "Put int into string at index"
     "string index int" "-")
    ("getinterval"
     "Return substring of string starting at index for count elements"
     "string index count" "substring")
    ("putinterval"
     "Replace substring of string1 starting at index by string2"
     "string1 index string2" "-")
    ("copy"
     "Copy elements of string1 to initial substring of string2"
     "string1 string2" "substring2")
    ("forall"
     "Execute proc for each element of string"
     "string proc" "-")
    ("anchorsearch"
     "Search for seek at start of string or string false"
     "string seek" "post match true")
    ("search"
     "Search for seek in string or string false"
     "string seek" "post match pre true")
    ("token"
     "Read token from start of string or false"
     "string" "post any true")
    ;;
    ;; Relational, Boolean, and Bitwise Operators
    ;;
    ("eq"
     "Test equal"
     "any1 any2" "bool")
    ("ne"
     "Test not equal"
     "any1 any2" "bool")
    ("ge"
     "Test greater than or equal"
     "num1 | str1 num2 | str2" "bool")
    ("gt"
     "Test greater than"
     "num1 | str1 num2 | str2" "bool")
    ("le"
     "Test less than or equal"
     "num1 | str1 num2 | str2" "bool")
    ("lt"
     "Test less than"
     "num1 | str1 num2 | str2" "bool")
    ("and"
     "Perform logical|bitwise and"
     "bool1 | int1 bool2 | int2" "bool3 | int3")
    ("not"
     "Perform logical|bitwise not"
     "bool1 | int1" "bool2 | int2")
    ("or"
     "Perform logical|bitwise inclusive or"
     "bool1 | int1 bool2 | int2" "bool3 | int3")
    ("xor"
     "Perform logical|bitwise exclusive or"
     "bool1 | int1 bool2 | int2" "bool3 | int3")
    ("true"
     "Return boolean value true"
     "-" "true")
    ("false"
     "Return boolean value false"
     "-" "false")
    ("bitshift"
     "Perform bitwise shift of int1 (positive is left)"
     "int1 shift" "int2")
    ;;
    ;; Control Operators
    ;;
    ("exec"
     "Execute arbitrary object"
     "any" "-")
    ("if"
     "Execute proc if bool is true"
     "bool proc" "-")
    ("ifelse"
     "Execute proc1 if bool is true, proc2 if false"
     "bool proc1 proc2" "-")
    ("for"
     "Execute proc with values from initial by steps of increment to limit"
     "initial increment limit proc" "-")
    ("repeat"
     "Execute proc int times"
     "int proc" "-")
    ("loop"
     "Execute proc an indefinite number of times"
     "proc" "-")
    ("exit"
     "Exit innermost active loop"
     "-" "-")
    ("stop"
     "Terminate stopped context"
     "-" "-")
    ("stopped"
     "Establish context for catching stop"
     "any" "bool")
    ("countexecstack"
     "Count elements on execution stack"
     "-" "int")
    ("execstack"
     "Copy execution stack into array"
     "array" "subarray")
    ("quit"
     "Terminate interpreter"
     "-" "-")
    ("start"
     "Executed at interpreter startup"
     "-" "-")
    ;;
    ;; Type, Attribute, and Conversion Operators
    ;;
    ("type"
     "Return type of any"
     "any" "name")
    ("cvlit"
     "Make object literal"
     "any" "any")
    ("cvx"
     "Make object executable"
     "any" "any")
    ("xcheck"
     "Test executable attribute"
     "any" "bool")
    ("executeonly"
     "Reduce access to execute-only"
     "array | packedarray | file | string" "array | packedarray | file | string")
    ("noaccess"
     "Disallow any access"
     "array | packedarray | dict | file | string" "array | packedarray | dict | file | string")
    ("readonly"
     "Reduce access to read-only"
     "array | packedarray | dict | file | string" "array | packedarray | dict | file | string")
    ("rcheck"
     "Test read access"
     "array | packedarray | dict | file | string" "bool")
    ("wcheck"
     "Test write access"
     "array | packedarray | dict | file | string" "bool")
    ("cvi"
     "Convert to integer"
     "num | string" "int")
    ("cvn"
     "Convert to name"
     "string" "name")
    ("cvr"
     "Convert to real"
     "num | string" "real")
    ("cvrs"
     "Convert with radix to string"
     "num radix string" "substring")
    ("cvs"
     "Convert to string"
     "any string" "substring")
    ;;
    ;; File Operators
    ;;
    ("file"
     "Open named file with specified access"
     "filename access" "file")
    ("filter"
     "Establish filtered file"
     "datasrc | datatgt dict param1 ... paramn filtername" "file")
    ("closefile"
     "Close file"
     "file" "-")
    ("read"
     "Read one character from file or false"
     "file" "int true")
    ("write"
     "Write one character to file"
     "file int" "-")
    ("readhexstring"
     "Read hexadecimal numbers from file into string"
     "file string" "substring bool")
    ("writehexstring"
     "Write string to file as hexadecimal"
     "file string" "-")
    ("readstring"
     "Read string from file"
     "file string" "substring bool")
    ("writestring"
     "Write string to file"
     "file string" "-")
    ("readline"
     "Read line from file into string"
     "file string" "substring bool")
    ("token"
     "Read token from file or false"
     "file" "any true")
    ("bytesavailable"
     "Return number of bytes available to read"
     "file" "int")
    ("flush"
     "Send buffered data to standard output file"
     "-" "-")
    ("flushfile"
     "Send buffered data or read to EOF"
     "file" "-")
    ("resetfile"
     "Discard buffered characters"
     "file" "-")
    ("status"
     "Return status of file (true=valid)"
     "file" "bool")
    ("status"
     "Return information about named file"
     "filename" "pages bytes referenced created true or false")
    ("run"
     "Execute contents of named file"
     "filename" "-")
    ("currentfile"
     "Return file currently being executed"
     "-" "file")
    ("deletefile"
     "Delete named file"
     "filename" "-")
    ("renamefile"
     "Rename file filename1 to filename2"
     "filename1 filename2" "-")
    ("filenameforall"
     "Execute proc for each file name matching template"
     "template proc scratch" "-")
    ("setfileposition"
     "Set file to specified position"
     "file position" "-")
    ("fileposition"
     "Return current position in file"
     "file" "position")
    ("print"
     "Write string to standard output file"
     "string" "-")
    ("="
     "Write text representation of any to standard output file"
     "any" "-")
    ("=="
     "Write syntactic representation of any to standard output file"
     "any" "-")
    ("stack"
     "Print stack nondestructively using ="
     "any1 ... anyn" "any1 ... anyn")
    ("pstack"
     "Print stack nondestructively using =="
     "any1 ... anyn" "any1 ... anyn")
    ("printobject"
     "Write binary object to standard output file, using tag"
     "obj tag" "-")
    ("writeobject"
     "Write binary object to file, using tag"
     "file obj tag" "-")
    ("setobjectformat"
     "Set binary object format (0=disable, 1=IEEE high, 2=IEEE low, 3=native high, 4=native low)"
     "int" "-")
    ("currentobjectformat"
     "Return binary object format"
     "-" "int")
    ;;
    ;; Resource Operators
    ;;
    ("defineresource"
     "Register named resource instance in category"
     "key instance category" "instance")
    ("undefineresource"
     "Remove resource registration"
     "key category" "-")
    ("findresource"
     "Return resource instance identified by key in category"
     "key category" "instance")
    ("findcolorrendering"
     "Select CIE-based color rendering dictionary by rendering intent"
     "renderingintent" "name bool")
    ("resourcestatus"
     "Return status of resource instance or false"
     "key category" "status size true")
    ("resourceforall"
     "Enumerate resource instances in category"
     "template proc scratch category" "-")
    ;;
    ;; Virtual Memory Operators
    ;;
    ("save"
     "Create VM snapshot"
     "-" "save")
    ("restore"
     "Restore VM snapshot"
     "save" "-")
    ("setglobal"
     "Set VM allocation mode (false=local, true=global)"
     "bool" "-")
    ("currentglobal"
     "Return current VM allocation mode"
     "-" "bool")
    ("gcheck"
     "Return true if any is simple or in global VM, false if in local VM"
     "any" "bool")
    ("startjob"
     "Start new job that will alter initial VM if bool1 is true"
     "bool1 password" "bool2")
    ("defineuserobject"
     "Define user object associated with index"
     "index any" "-")
    ("execuserobject"
     "Execute user object associated with index"
     "index" "-")
    ("undefineuserobject"
     "Remove user object associated with index"
     "index" "-")
    ("UserObjects"
     "Return current UserObjects array defined in userdict"
     "-" "array")
    ;;
    ;; Miscellaneous Operators
    ;;
    ("bind"
     "Replace operator names in proc with operators; perform idiom recognition"
     "proc" "proc")
    ("null"
     "Push null on stack"
     "-" "null")
    ("version"
     "Return interpreter version"
     "-" "string")
    ("realtime"
     "Return real time in milliseconds"
     "-" "int")
    ("usertime"
     "Return execution time in milliseconds"
     "-" "int")
    ("languagelevel"
     "Return LanguageLevel"
     "-" "int")
    ("product"
     "Return product name"
     "-" "string")
    ("revision"
     "Return product revision level"
     "-" "int")
    ("serialnumber"
     "Return machine serial number"
     "-" "int")
    ("executive"
     "Invoke interactive executive"
     "-" "-")
    ("echo"
     "Turn echoing on or off"
     "bool" "-")
    ("prompt"
     "Executed when ready for interactive input"
     "-" "-")
    ;;
    ;; Graphics State Operators (Device-Independent)
    ;;
    ("gsave"
     "Push graphics state"
     "-" "-")
    ("grestore"
     "Pop graphics state"
     "-" "-")
    ("clipsave"
     "Push clipping path"
     "-" "-")
    ("cliprestore"
     "Pop clipping path"
     "-" "-")
    ("grestoreall"
     "Pop to bottommost graphics state"
     "-" "-")
    ("initgraphics"
     "Reset graphics state parameters"
     "-" "-")
    ("gstate"
     "Create graphics state object"
     "-" "gstate")
    ("setgstate"
     "Set graphics state from gstate"
     "gstate" "-")
    ("currentgstate"
     "Copy current graphics state into gstate"
     "gstate" "gstate")
    ("setlinewidth"
     "Set line width"
     "num" "-")
    ("currentlinewidth"
     "Return current line width"
     "-" "num")
    ("setlinecap"
     "Set shape of line ends for stroke (0=butt, 1=round, 2=square)"
     "int" "-")
    ("currentlinecap"
     "Return current line cap"
     "-" "int")
    ("setlinejoin"
     "Set shape of corners for stroke (0=miter, 1=round, 2=bevel)"
     "int" "-")
    ("currentlinejoin"
     "Return current line join"
     "-" "int")
    ("setmiterlimit"
     "Set miter length limit"
     "num" "-")
    ("currentmiterlimit"
     "Return current miter limit"
     "-" "num")
    ("setstrokeadjust"
     "Set stroke adjustment (false=disable, true=enable)"
     "bool" "-")
    ("currentstrokeadjust"
     "Return current stroke adjustment"
     "-" "bool")
    ("setdash"
     "Set dash pattern for stroking"
     "array offset" "-")
    ("currentdash"
     "Return current dash pattern"
     "-" "array offset")
    ("setcolorspace"
     "Set color space"
     "array | name" "-")
    ("currentcolorspace"
     "Return current color space"
     "-" "array")
    ("setcolor"
     "Set color components"
     "comp1 ... compn" "-")
    ("setcolor"
     "Set colored tiling pattern as current color"
     "pattern" "-")
    ("setcolor"
     "Set uncolored tiling pattern as current color"
     "comp1 ... compn pattern" "-")
    ("currentcolor"
     "Return current color components"
     "-" "comp1 ... compn")
    ("setgray"
     "Set color space to DeviceGray and color to specified gray value (0=black, 1=white)"
     "num" "-")
    ("currentgray"
     "Return current color as gray value"
     "-" "num")
    ("sethsbcolor"
     "Set color space to DeviceRGB and color to specified hue, saturation, brightness"
     "hue saturation brightness" "-")
    ("currenthsbcolor"
     "Return current color as hue, saturation, brightness"
     "-" "hue saturation brightness")
    ("setrgbcolor"
     "Set color space to DeviceRGB and color to specified red, green, blue"
     "red green blue" "-")
    ("currentrgbcolor"
     "Return current color as red, green, blue"
     "-" "red green blue")
    ("setcmykcolor"
     "Set color space to DeviceCMYK and color to specified cyan, magenta, yellow, black"
     "cyan magenta yellow black" "-")
    ("currentcmykcolor"
     "Return current color as cyan, magenta, yellow, black"
     "-" "cyan magenta yellow black")
    ;;
    ;; Graphics State Operators (Device-Dependent)
    ;;
    ("sethalftone"
     "Set halftone dictionary"
     "halftone" "-")
    ("currenthalftone"
     "Return current halftone dictionary"
     "-" "halftone")
    ("setscreen"
     "Set gray halftone screen by frequency, angle, and spot function"
     "frequency angle proc" "-")
    ("setscreen"
     "Set gray halftone screen from halftone dictionary"
     "frequency angle halftone" "-")
    ("currentscreen"
     "Return current gray halftone screen"
     "-" "frequency angle proc | halftone")
    ("setcolorscreen"
     "Set all four halftone screens"
     "redfreq redang redproc | redhalftone
greenfreq greenang greenproc | greenhalftone
bluefreq blueang blueproc | bluehalftone
grayfreq grayang grayproc | grayhalftone" "-")
    ("currentcolorscreen"
     "Return all four halftone screens"
     "-" "redfreq redang redproc | redhalftone
greenfreq greenang greenproc | greenhalftone
bluefreq blueang blueproc | bluehalftone
grayfreq grayang grayproc | grayhalftone")
    ("settransfer"
     "Set gray transfer function"
     "proc" "-")
    ("currenttransfer"
     "Return current gray transfer function"
     "-" "proc")
    ("setcolortransfer"
     "Set all four transfer functions"
     "redproc greenproc blueproc grayproc" "-")
    ("currentcolortransfer"
     "Return current transfer functions"
     "-" "redproc greenproc blueproc grayproc")
    ("setblackgeneration"
     "Set black-generation function"
     "proc" "-")
    ("currentblackgeneration"
     "Return current black-generation function"
     "-" "proc")
    ("setundercolorremoval"
     "Set undercolor-removal function"
     "proc" "-")
    ("currentundercolorremoval"
     "Return current undercolor-removal function"
     "-" "proc")
    ("setcolorrendering"
     "Set CIE-based color rendering dictionary"
     "dict" "-")
    ("currentcolorrendering"
     "Return current CIE-based color rendering dictionary"
     "-" "dict")
    ("setflat"
     "Set flatness tolerance"
     "num" "-")
    ("currentflat"
     "Return current flatness"
     "-" "num")
    ("setoverprint"
     "Set overprint parameter"
     "bool" "-")
    ("currentoverprint"
     "Return current overprint parameter"
     "-" "bool")
    ("setsmoothness"
     "Set smoothness parameter"
     "num" "-")
    ("currentsmoothness"
     "Return current smoothness parameter"
     "-" "num")
    ;;
    ;; Coordinate System and Matrix Operators
    ;;
    ("matrix"
     "Create identity matrix"
     "-" "matrix")
    ("initmatrix"
     "Set CTM to device default"
     "-" "-")
    ("identmatrix"
     "Fill matrix with identity transform"
     "matrix" "matrix")
    ("defaultmatrix"
     "Fill matrix with device default matrix"
     "matrix" "matrix")
    ("currentmatrix"
     "Fill matrix with CTM"
     "matrix" "matrix")
    ("setmatrix"
     "Replace CTM by matrix"
     "matrix" "-")
    ("translate"
     "Translate user space by (tx , ty)"
     "tx ty" "-")
    ("translate"
     "Define translation by (tx , ty)"
     "tx ty matrix" "matrix")
    ("scale"
     "Scale user space by sx and sy"
     "sx sy" "-")
    ("scale"
     "Define scaling by sx and sy"
     "sx sy matrix" "matrix")
    ("rotate"
     "Rotate user space by angle degrees"
     "angle" "-")
    ("rotate"
     "Define rotation by angle degrees"
     "angle matrix" "matrix")
    ("concat"
     "Replace CTM by matrix * CTM"
     "matrix" "-")
    ("concatmatrix"
     "Fill matrix3 with matrix1 * matrix2"
     "matrix1 matrix2 matrix3" "matrix3")
    ("transform"
     "Transform (x, y) by CTM"
     "x y" "x' y'")
    ("transform"
     "Transform (x, y) by matrix"
     "x y matrix" "x' y'")
    ("dtransform"
     "Transform distance (dx, dy) by CTM"
     "dx dy" "dx' dy'")
    ("dtransform"
     "Transform distance (dx, dy) by matrix"
     "dx dy matrix" "dx' dy'")
    ("itransform"
     "Perform inverse transform of (x', y') by CTM"
     "x' y'" "xy")
    ("itransform"
     "Perform inverse transform of (x', y') by matrix"
     "x' y' matrix" "xy")
    ("idtransform"
     "Perform inverse transform of distance (dx', dy') by CTM"
     "dx' dy'" "dx dy")
    ("idtransform"
     "Perform inverse transform of distance (dx', dy') by matrix"
     "dx' dy' matrix" "dx dy")
    ("invertmatrix"
     "Fill matrix2 with inverse of matrix1"
     "matrix1 matrix2" "matrix2")
    ;;
    ;; Path Construction Operators
    ;;
    ("newpath"
     "Initialize current path to be empty"
     "-" "-")
    ("currentpoint"
     "Return current point coordinates"
     "-" "xy")
    ("moveto"
     "Set current point to (x, y)"
     "x y" "-")
    ("rmoveto"
     "Perform relative moveto"
     "dx dy" "-")
    ("lineto"
     "Append straight line to (x, y)"
     "x y" "-")
    ("rlineto"
     "Perform relative lineto"
     "dx dy" "-")
    ("arc"
     "Append counterclockwise arc"
     "x y r angle1 angle2" "-")
    ("arcn"
     "Append clockwise arc"
     "x y r angle1 angle2" "-")
    ("arct"
     "Append tangent arc"
     "x1 y1 x2 y2 r" "-")
    ("arcto"
     "Append tangent arc"
     "x1 y1 x2 y2 r" "xt1 yt1 xt2 yt2")
    ("curveto"
     "Append Bézier cubic section"
     "x1 y1 x2 y2 x3 y3" "-")
    ("rcurveto"
     "Perform relative curveto"
     "dx1 dy1 dx2 dy2 dx3 dy3" "-")
    ("closepath"
     "Connect subpath back to its starting point"
     "-" "-")
    ("flattenpath"
     "Convert curves to sequences of straight lines"
     "-" "-")
    ("reversepath"
     "Reverse direction of current path"
     "-" "-")
    ("strokepath"
     "Compute outline of stroked path"
     "-" "-")
    ("ustrokepath"
     "Compute outline of stroked userpath"
     "userpath" "-")
    ("ustrokepath"
     "Compute outline of stroked userpath"
     "userpath matrix" "-")
    ("charpath"
     "Append glyph outline to current path"
     "string bool" "-")
    ("uappend"
     "Interpret userpath and append to current path"
     "userpath" "-")
    ("clippath"
     "Set current path to clipping path"
     "-" "-")
    ("setbbox"
     "Set bounding box for current path"
     "llx lly urx ury" "-")
    ("pathbbox"
     "Return bounding box of current path"
     "-" "llx lly urx ury")
    ("pathforall"
     "Enumerate current path"
     "move line curve close" "-")
    ("upath"
     "Create userpath for current path; include ucache if bool is true"
     "bool" "userpath")
    ("initclip"
     "Set clipping path to device default"
     "-" "-")
    ("clip"
     "Clip using nonzero winding number rule"
     "-" "-")
    ("eoclip"
     "Clip using even-odd rule"
     "-" "-")
    ("rectclip"
     "Clip with rectangular path"
     "x y width height" "-")
    ("rectclip"
     "Clip with rectangular paths"
     "numarray | numstring" "-")
    ("ucache"
     "Declare that user path is to be cached"
     "-" "-")
    ;;
    ;; Painting Operators
    ;;
    ("erasepage"
     "Paint current page white"
     "-" "-")
    ("stroke"
     "Draw line along current path"
     "-" "-")
    ("fill"
     "Fill current path with current color"
     "-" "-")
    ("eofill"
     "Fill using even-odd rule"
     "-" "-")
    ("rectstroke"
     "Define rectangular path and stroke"
     "x y width height" "-")
    ("rectstroke"
     "Define rectangular path, concatenate matrix, and stroke"
     "x y width height matrix" "-")
    ("rectstroke"
     "Define rectangular paths and stroke"
     "numarray | numstring" "-")
    ("rectstroke"
     "Define rectangular paths, concatenate matrix, and stroke"
     "numarray | numstring matrix" "-")
    ("rectfill"
     "Fill rectangular path"
     "x y width height" "-")
    ("rectfill"
     "Fill rectangular paths"
     "numarray | numstring" "-")
    ("ustroke"
     "Interpret and stroke userpath"
     "userpath" "-")
    ("ustroke"
     "Interpret userpath, concatenate matrix, and stroke"
     "userpath matrix" "-")
    ("ufill"
     "Interpret and fill userpath"
     "userpath" "-")
    ("ueofill"
     "Fill userpath using even-odd rule"
     "userpath" "-")
    ("shfill"
     "Fill area defined by shading pattern"
     "dict" "-")
    ("image"
     "Paint any sampled image"
     "dict" "-")
    ("image"
     "Paint monochrome sampled image width height bits/comp matrix"
     "idth height bits/sample matrix datasrc" "-")
    ("colorimage"
     "Paint color sampled image"
     "atasrc0 ... datasrcncomp-1 multi ncomp" "-")
    ("imagemask"
     "Paint current color through mask"
     "dict" "-")
    ("imagemask"
     "Paint current color through mask"
     "width height polarity matrix datasrc" "-")
    ;;
    ;; Insideness-Testing Operators
    ;;
    ("infill"
     "Test whether (x, y) would be painted by fill"
     "x y" "bool")
    ("infill"
     "Test whether pixels in userpath would be painted by fill"
     "userpath" "bool")
    ("ineofill"
     "Test whether (x, y) would be painted by eofill"
     "x y" "bool")
    ("ineofill"
     "Test whether pixels in userpath would be painted by eofill"
     "userpath" "bool")
    ("inufill"
     "Test whether (x, y) would be painted by ufill of userpath"
     "x y userpath" "bool")
    ("inufill"
     "Test whether pixels in userpath1 would be painted by ufill of userpath2"
     "userpath1 userpath2" "bool")
    ("inueofill"
     "Test whether (x, y) would be painted by ueofill of userpath"
     "x y userpath" "bool")
    ("inueofill"
     "Test whether pixels in userpath1 would be painted by ueofill of userpath2"
     "userpath1 userpath2" "bool")
    ("instroke"
     "Test whether (x, y) would be painted by stroke"
     "x y" "bool")
    ("inustroke"
     "Test whether (x, y) would be painted by ustroke of userpath"
     "x y userpath" "bool")
    ("inustroke"
     "Test whether (x, y) would be painted by ustroke of userpath"
     "x y userpath matrix" "bool")
    ("inustroke"
     "Test whether pixels in userpath1 would be painted by ustroke of userpath2"
     "userpath1 userpath2" "bool")
    ("inustroke"
     "Test whether pixels in userpath1 would be painted by ustroke of userpath2"
     "userpath1 userpath2 matrix" "bool")
    ;;
    ;; Form and Pattern Operators
    ;;
    ("makepattern"
     "Create pattern instance from prototype"
     "pattern matrix" "pattern'")
    ("setpattern"
     "Install pattern as current color"
     "pattern" "-")
    ("setpattern"
     "Install pattern as current color"
     "comp1 ... compn pattern" "-")
    ("execform"
     "Paint form"
     "form" "-")
    ;;
    ;; Device Setup and Output Operators
    ;;
    ("showpage"
     "Transmit and reset current page"
     "-" "-")
    ("copypage"
     "Transmit current page"
     "-" "-")
    ("setpagedevice"
     "Install page-oriented output device"
     "dict" "-")
    ("currentpagedevice"
     "Return current page device parameters"
     "-" "dict")
    ("nulldevice"
     "Install no-output device"
     "-" "-")
    ;;
    ;; Glyph and Font Operators
    ;;
    ("definefont"
     "Register font | cidfont in Font resource category"
     "key font | cidfont" "font | cidfont")
    ("composefont"
     "Register composite font dictionary created from CMap and array of CIDFonts or fonts"
     "key name | string | dict array" "font")
    ("undefinefont"
     "Remove Font resource registration"
     "key" "-")
    ("findfont"
     "Return Font resource instance identified by key"
     "key" "font | cidfont")
    ("scalefont"
     "Scale font | cidfont by scale to produce font'| cidfont'"
     "font | cidfont scale" "font'| cidfont'")
    ("makefont"
     "Transform font | cidfont by matrix to produce font'| cidfont'"
     "font | cidfont matrix" "font'| cidfont'")
    ("setfont"
     "Set font or CIDFont in graphics state"
     "font | cidfont" "-")
    ("rootfont"
     "Return last set font or CIDFont"
     "-" "font | cidfont")
    ("currentfont"
     "Return current font or CIDFont, possibly a descendant of rootfont"
     "-" "font | cidfont")
    ("selectfont"
     "Set font or CIDFont given name and transform"
     "key scale | matrix" "-")
    ("show"
     "Paint glyphs for string in current font"
     "string" "-")
    ("ashow"
     "Add (ax , ay) to width of each glyph while showing string"
     "ax ay string" "-")
    ("widthshow"
     "Add (cx , cy) to width of glyph for char while showing string"
     "cx cy char string" "-")
    ("awidthshow"
     "Combine effects of ashow and widthshow"
     "cx cy char ax ay string" "-")
    ("xshow"
     "Paint glyphs for string using x widths in numarray | numstring"
     "string numarray | numstring" "-")
    ("xyshow"
     "Paint glyphs for string using x and y widths in numarray | numstring"
     "string numarray | numstring" "-")
    ("yshow"
     "Paint glyphs for string using y widths in numarray | numstring"
     "string numarray | numstring" "-")
    ("glyphshow"
     "Paint glyph for character identified by name | cid"
     "name | cid" "-")
    ("stringwidth"
     "Return width of glyphs for string in current font"
     "string" "wx wy")
    ("cshow"
     "Invoke character mapping algorithm and call proc"
     "proc string" "-")
    ("kshow"
     "Execute proc between characters shown from string"
     "proc string" "-")
    ("FontDirectory"
     "Return dictionary of Font resource instances"
     "-" "dict")
    ("GlobalFontDirectory"
     "Return dictionary of Font resource instances in global VM"
     "-" "dict")
    ("StandardEncoding"
     "Return Adobe standard font encoding vector"
     "-" "array")
    ("ISOLatin1Encoding"
     "Return ISO Latin-1 font encoding vector"
     "-" "array")
    ("findencoding"
     "Find encoding vector"
     "key" "array")
    ("setcachedevice"
     "Declare cached glyph metrics"
     "wx wy llx lly urx ury" "-")
    ("setcachedevice2"
     "Declare cached glyph metrics"
     "w0x w0y llx lly urx ury w1x w1y vx vy" "-")
    ("setcharwidth"
     "Declare uncached glyph metrics"
     "wx wy" "-")
    ;;
    ;; Interpreter Parameter Operators
    ;;
    ("setsystemparams"
     "Set systemwide interpreter parameters"
     "dict" "-")
    ("currentsystemparams"
     "Return systemwide interpreter parameters"
     "-" "dict")
    ("setuserparams"
     "Set per-context interpreter parameters"
     "dict" "-")
    ("currentuserparams"
     "Return per-context interpreter parameters"
     "-" "dict")
    ("setdevparams"
     "Set parameters for input/output device"
     "string dict" "-")
    ("currentdevparams"
     "Return device parameters"
     "string" "dict")
    ("vmreclaim"
     "Control garbage collector"
     "int" "-")
    ("setvmthreshold"
     "Control garbage collector"
     "int" "-")
    ("vmstatus"
     "Report VM status"
     "-" "level used maximum")
    ("cachestatus"
     "Return font cache status and parameters"
     "-" "bsize bmax msize mmax csize cmax blimit")
    ("setcachelimit"
     "Set maximum bytes in cached glyph"
     "int" "-")
    ("setcacheparams"
     "Set font cache parameters"
     "mark size lower upper" "-")
    ("currentcacheparams"
     "Return current font cache parameters"
     "-" "mark size lower upper")
    ("setucacheparams"
     "Set user path cache parameters"
     "mark blimit" "-")
    ("ucachestatus"
     "Return user path cache status and parameters"
     "-" "mark bsize bmax rsize rmax blimit"))
  "Operators and the operand stack before and after execution.
Note: The alist contains duplicate keys since some operators (e.g.
transform) can process different shapes of the operator stack.  At the
moment only the first entry is used to generate the ElDoc string.  This
is a known issue.")

(defvar ps-ts-operator-summary-obarray
  (let ((operators (obarray-make 255)))
    (dolist (elem ps-ts-operator-summary-alist)
      (if (not (intern-soft (car elem) operators))
          (let ((sym (intern (car elem) operators)))
            ;; store documentation string in standard property
            (put sym 'variable-documentation (cadr elem))
            ;; store value; this will be a list with two elements
            (set sym (cddr elem)))))
    operators)
  "Operators and the operand stack before and after execution.
The variable `ps-ts-operator-summary-alist' is used to create this
obarray.")

(defun ps-ts-mode-eldoc-operator (&rest _unused)
  "Return operator at point with pre- and post-operation stack."
  (let ((node (treesit-node-at (point))))
    (if (and (treesit-node-p node)
             (member (treesit-node-type node) '("operator" "[" "]" "<<" ">>"))
             (<= (treesit-node-start node) (point))
             (<= (point) (treesit-node-end node)))
        (let ((sym (intern-soft (treesit-node-text node)
                                ps-ts-operator-summary-obarray)))
          (if sym
              (concat (car (symbol-value sym)) "  "
                      (treesit-node-text node) "  "
                      (cadr (symbol-value sym))))))))


;; Completion

(defvar ps-ts-completion-operators
  (seq-uniq (mapcar #'car ps-ts-operator-summary-alist))
  "The operator names that `completion-at-point' will suggest.")

(defun ps-ts-mode-complete-operator ()
  "Return operator names for `completion-at-point'.
This function should be added to `completion-at-point-functions'."
  (interactive "*")
  (let ((node (treesit-node-at (point))))
    (if (and (treesit-node-p node)
             (equal (treesit-node-type node) "operator"))
        (list (treesit-node-start node)
              (treesit-node-end node)
              (completion-table-dynamic
               (lambda (_)
                 ps-ts-completion-operators))))))

(defun ps-ts-mode-complete-font ()
  "Return font names for `completion-at-point'."
  (interactive "*")
  (let ((node (treesit-node-at (point))))
    (if (and (treesit-node-p node)
             (equal (treesit-node-type node) "literal"))
        (list (treesit-node-start node)
              (treesit-node-end node)
              (completion-table-dynamic
               (lambda (_)
                 ps-ts-font-names))))))


;; Font-Lock

(defvar ps-ts--operators-regex
  (regexp-opt ps-ts-important-operators 'symbols)
  "Regular expression matching important operator names.")

(defvar ps-ts--builtin-regex
  (regexp-opt (mapcar #'car ps-ts-operator-summary-alist) 'symbols)
  "Regular expression matching all operator names.")

(defvar ps-ts--constants-regex
  (regexp-opt ps-ts-special-operators 'symbols)
  "Regular expression matching all constant names.")

(defvar ps-ts--fonts-regex
  (regexp-opt ps-ts-font-names 'symbols)
  "Regular expression matching PostScript fonts.")

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
     ;; composite objects
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ">>") parent-bol 0)
     ((parent-is "array") parent-bol ps-ts-indent-level)
     ((parent-is "procedure") parent-bol ps-ts-indent-level)
     ((parent-is "dictionary") parent-bol ps-ts-indent-level)
     ;; use indentation from previous line by default
     (catch-all prev-line 0)))
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
comments, strings, keywords) is available.  Customize the variable
`treesit-font-lock-level' to control the level of decoration.

Indentation is implemented for array, procedure and dictionary elements.
Customize `ps-ts-indent-level' to set the level of indentation to use.

The mode supports ElDoc to display up the pre- and post-execution
operator stack for the operator at point.  The echo area shows the
operator stack before the execution, the named operator and then the
operator stack after the execution (e.g.: \"num1 num2 add sum\").  The
same representation is used in the Red Book, the PostScript Language
Reference Manual.

The function `completion-at-point' can complete the regular PostScript
operators and the 35 standard PostScript font names.

The `doc-view-minor-mode' is automatically activated unless the
customization variable `ps-ts-inhibit-doc-view' is non-nil.

The mode needs the Tree-sitter parser for PostScript code.  A parser
suitable for the current package version can be installed using the
function `ps-ts-mode-install-grammar'.  Some development tools (C
compiler, ...) are required for this.

Indentation and fontification depend on the concrete syntax tree
returned by the Tree-sitter parser.  So errors like a missing closing
parenthesis or bracket can lead to wrong indentation or missing
fontification.  This is easily resolved by fixing the particular syntax
error.

\\{ps-ts-mode-map}"
  (setq-local require-final-newline mode-require-final-newline)

  ;; Comments
  (setq-local comment-start "%")
  (setq-local comment-end "")
  (setq-local comment-start-skip "%+[ \t]*")

  ;; DocView minor mode allows to view the current PostScript file
  (unless ps-ts-inhibit-doc-view
    (doc-view-minor-mode 1))

  ;; Treesitter
  (when (treesit-ready-p 'postscript)
    (treesit-parser-create 'postscript)

    ;; Navigation
    (setq treesit-defun-type-regexp "\\`document_structure_comment\\'")

    ;; Font-Lock
    (setq treesit-font-lock-feature-list ps-ts-mode-feature-list
          treesit-font-lock-settings     (apply #'treesit-font-lock-rules
                                                ps-ts-mode-font-lock-settings))

    ;; Indentation
    (setq indent-tabs-mode            ps-ts-indent-tabs-mode
          treesit-simple-indent-rules ps-ts-indent-rules)

    ;; Eldoc
    (add-hook 'eldoc-documentation-functions #'ps-ts-mode-eldoc-operator nil t)

    ;; Completion
    (add-hook 'completion-at-point-functions #'ps-ts-mode-complete-operator nil t)
    (add-hook 'completion-at-point-functions #'ps-ts-mode-complete-font nil t)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.e?ps\\'" . ps-ts-mode))

;;;###autoload
(add-to-list 'magic-mode-alist '("^%!PS\\>" . ps-ts-mode))

(provide 'ps-ts-mode)

;;; ps-ts-mode.el ends here
