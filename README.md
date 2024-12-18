# Emacs major mode for PostScript using Tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/ps-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/ps-ts-mode/actions/workflows/CI.yaml)

This is a major mode for [GNU Emacs](https://www.gnu.org/software/emacs/) 29.1 or later which adds support for the PostScript page description and programming language. The mode uses a Tree-sitter parser to be able to parse the code and provide fontification, indentation, navigation and more.

## Features

The mode provides the following features and enhancements to make writing PostScript code easier.

### Syntax highlighting

Syntax highlighting for the following elements is implemented:

- document structure comments
- comments
- numbers
- strings including escape sequences
- literal names
- some important operators
- the standard builtin font names

### Indentation

Indentation for array, procedure and dictionary structures is implemented.

### Navigation

The keybindings <kbd>C-M-a</kbd> and <kbd>C-M-e</kbd> jump to
preceding or following document structure comment. In a DSC compliant
document this includes the prolog, setup and page sections.

## Installation

Emacs 29.1 or above with Tree-sitter support is required.

Also the appropriate
[parser](https://github.com/smoeding/tree-sitter-postscript) for the
Pic language needs to be installed. The following Elisp code should be
used to install the PostScript language parser.  This requires some tools -- notably a compiler toolchain -- to be available on your machine.

```elisp
(require 'ps-ts-mode)
(ps-ts-mode-install-grammar)
```

Using the function provided by the package ensures that a version of the parser matching the package will be installed. These commands should also be used to update the parser to the correct version when the package is updated.

## License

PostScript Tree-sitter Mode is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

PostScript Tree-sitter Mode is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the [GNU General Public License](http://www.gnu.org/licenses/) for more details.

PostScript is a trademark of Adobe Inc.
