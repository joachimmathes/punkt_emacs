;;; t3php-mode.el --- An Emacs major mode for editing TYPO3 php files

;; Copyright (C) 2009  Joachim Mathes
;;
;; Author: Joachim Mathes <joachim <underscore> mathes <at> web <dot> de>
;; Created: August 2009
;; Version: 0.0.1 $Id$
;; Keywords: files

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a major mode for editing TYPO3 PHP files.  It is developed to
;; support syntax highlighting and code creation.

;; Installation:

;; To install just drop this file into a directory on your load-path and
;; byte-compile it.  To set up Emacs to automatically edit files ending in ".ts"
;; using ts-mode add the following to your ~/.emacs file (GNU Emacs) or
;; ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.php$" . t3php-mode) auto-mode-alist))
;;    (autoload 't3php-mode "t3php-mode" "TYPO3 php file editing mode." t)

;; This code is loosely based on and inspired by`php-mode.el' - a major mode
;; for editing PHP code. So, my special thanks go to Turadg Aleahmad who is
;; the original author of `php-mode.el' and Aaron S. Hawley, its current
;; maintainer.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(defconst t3php-version "0.0.1"
  "`t3php-mode' version number.")

;;;; User definable variables

(defgroup t3php nil
  "Major mode for editing TYPO3 PHP files."
  :prefix "t3php-"
  :group 'languages)

(defcustom t3php-author "Lisa Fremont <lisa@fremont.de>"
  "Author for php-doc."
  :type 'string
  :group 't3php)

(defcustom t3php-typo3-extension-directory "ext/"
  "TYPO3 extension directory."
  :type 'string
  :group 't3php)

(defcustom t3php-path-to-typo3-extension-directory "typo3conf/"
  "TYPO3 extension directory."
  :type 'string
  :group 't3php)

(defcustom t3php-date-format "%Y-%m-%d"
  "Date format for php-doc.
See `format-time-string' function for further detail."
  :type 'string
  :group 't3php)

(defcustom t3php-year-format "%Y"
  "Year format for php-doc.
See `format-time-string' function for further detail."
  :type 'string
  :group 't3php)

(defcustom t3php-block-indentation 4
  "The indentation relative to a predecessing line which begins a new block."
  :type 'integer
  :group 'typoscript)

(defcustom t3php-newline-function 'newline-and-indent
  "Function to be called upon pressing `RET'."
  :type '(choice (const newline)
		 (const newline-and-indent)
		 (const reindent-then-newline-and-indent))
  :group 't3php)

(defcustom t3php-phpdoc-align t
  "Alignment of PHPDoc parameters."
  :type 'boolean
  :group 't3php)

(defcustom t3php-php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 't3php)

(defcustom t3php-php-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 't3php)

(defcustom t3php-outline-keep-other-windows t
  "If true, split the selected window to display the *t3php-outline* buffer.

When nil, all other windows except the selected one will be deleted, so that the
*t3php-outline* window fills half the frame."
  :type 'boolean
  :group 't3php)

(defcustom t3php-outline-split-windows-horizontally nil
  "If true, create outline windows by splitting horizontally.
Otherwise it is splitted vertically.

I must admit, that I sometimes get confused.  So if it is not for your
orientation, at least it is for mine.  :-)

Splitting a window `vertically' looks like this:
+-------+         +-------+
|       |         |       |
|       |  split  |       |
|       |  -----> +-------+
|       |    v    |       |
|       |         |       |
+-------+         +-------+

Splitting a window `horizontally' looks like this:
+-------+         +---+---+
|       |         |   |   |
|       |  split  |   |   |
|       |  -----> |   |   |
|       |    h    |   |   |
|       |         |   |   |
+-------+         +---+---+"
  :type 'boolean
  :group 't3php)

(defcustom t3php-outline-split-windows-fraction .3
  "Fraction of the width or height of the window to be used for outline window."
  :type 'number
  :group 't3php)

(defcustom t3php-outline-follow-mode nil
  "If true, point in *t3php-outline* will make PHP window to follow.

It will show the corresponding part of the document.  This flag can be toggled
from within the *t3php-outline* buffer with the `f' key."
  :type 'boolean
  :group 't3php)

(defcustom t3php-outline-hl-line-color "DarkSlateBlue"
  "The color used to highlight the horizontal line.

  The default value is `DarkSlateBlue'. For a list of all available colors use `M-x
list-colors-display"
  :type 'color
  :group 't3php)

(defcustom t3php-outline-method-name-color "sea green"
  "The color used to highlight method names in the OUTLINE.

  The default value is `sea green'.  For a list of all available colors use `M-x
`list-colors-display'"
  :type 'color
  :group 't3php)

;;;; Constants

(defconst t3php-php-tags
  (eval-when-compile
    (regexp-opt
     '("<?php" "?>" "<?" "<?="))))

(defconst t3php-keywords
  (eval-when-compile
    (regexp-opt
     ;; "class", "new" and "extends" get special treatment
     '("and" "as" "break" "continue" "case" "default" "declare" "do" "echo"
       "else" "elseif" "endfor" "endforeach" "endif" "endswitch" "endwhile"
       "exit" "extends" "implements" "for" "foreach" "global" "if" "include" "include_once"
       "next" "or" "require" "require_once" "return" "static" "switch" "then"
       "var" "while" "xor" "throw" "catch" "try" "instanceof" "catch all"
       "finally")))
  "PHP keywords.")

(defconst t3php-identifier
  (eval-when-compile
    '"[a-zA-Z\_][a-zA-Z0-9\_]*")
  "Characters in a PHP identifier.")

(defconst t3php-types
  (eval-when-compile
    (regexp-opt '("array" "bool" "boolean" "char" "const" "double" "float"
                  "int" "integer" "long" "mixed" "object" "real"
                  "string")))
  "PHP types.")

(defconst t3php-superglobals
  (eval-when-compile
    (regexp-opt '("_GET" "_POST" "_COOKIE" "_SESSION" "_ENV" "GLOBALS"
                  "_SERVER" "_FILES" "_REQUEST")))
  "PHP superglobal variables.")

(defconst t3php-font-lock-keywords-1
  (list
   ;; Fontify PHP tag
   (cons t3php-php-tags font-lock-preprocessor-face)

   ;; Fontify keywords
   (cons
    (concat "\\<\\(" t3php-keywords "\\)\\>")
    '(1 font-lock-keyword-face))
  "Low level highlighting for PHP mode."))

(defconst t3php-font-lock-keywords-2
  (append
   t3php-font-lock-keywords-1
   (list

    ;; Fontify class declaration
    '("\\<\\(class\\|interface\\)\\s-+\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))

    ;; Fontify function declaration
    '("\\<\\(function\\)\\s-+&?\\(\\sw+\\)\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))

    ;; Fontify class hierarchy
    '("\\<\\(self\\|parent\\)\\>" (1 font-lock-constant-face nil nil))

    ;; Fontify method and variable features
    '("\\<\\(private\\|protected\\|public\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; Fontify method features
    '("^\\s-*\\(abstract\\|static\\|final\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))

    ;; Fontify variable features
    '("^\\s-*\\(static\\|const\\)\\s-+\\$?\\sw+"
      (1 font-lock-keyword-face))
    ))
  "Medium level highlighting for PHP mode.")

(defconst t3php-font-lock-keywords-3
  (append
   t3php-font-lock-keywords-2
   (list

    ;; Fontify variables and function calls
    '("\\$\\(this\\|that\\)\\W" (1 font-lock-constant-face nil nil))
    `(,(concat "\\$\\(" t3php-superglobals "\\)\\W")
      (1 font-lock-constant-face nil nil)) ;; $_GET & co
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face)) ;; $variable
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face t t)) ;; ->variable
    '("->\\(\\sw+\\)\\s-*(" . (1 'default t t)) ;; ->function_call
    '("\\(\\sw+\\)::\\sw+\\s-*(?" . (1 font-lock-type-face)) ;; class::member
    '("::\\(\\sw+\\>[^(]\\)" . (1 'default)) ;; class::constant
    '("\\<\\sw+\\s-*[[(]" . 'default) ;; word( or word[
    '("\\<[0-9]+" . 'default) ;; number (also matches word)

    ;; Exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" t3php-types "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `(,(concat "[(,]\\s-*\\(\\sw+\\)\\s-+&?\\$\\sw+\\>")
      1 font-lock-type-face)

    ;; Warn about '$' immediately after ->
    '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
      (1 font-lock-warning-face) (2 'default))

    ;; Warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; Warn on any words not already fontified
    '("\\<\\sw+\\>" . font-lock-warning-face)))
  "High level highlighting for T3PHP mode.")

(defconst t3php-outline-buffer-name "*t3php-outline*"
  "Name of T3PHP OUTLINE buffer.")

(defconst t3php-outline-help
"Available keys in T3PHP OUTLINE buffer:
=====================================
n/<right>/<down> Move to next selectable item.
p/<left>/<up>    Move to previous selectable item.
<C-home>         Move to the beginning of the buffer.
<C-end>          Move too the end of the buffer.
SPC              Show the corresponding location of the TYPO3 PHP buffer.
TAB              Go to the location and keep the *t3php-outline* window.
RET              Go to the location and hide the *t3php-outline* window.
f                Toggle follow mode.
r                Reparse the PHP buffer.
q                Hide *t3php-outline* window.
q                Quit *t3php-outline* buffer.
?                Show this help side.")


;;;; Internal variables

(defvar t3php-mode-syntax-table nil
  "Syntax table used in T3PHP Mode buffers.")

(defvar t3php-mode-map ()
  "Key map used in T3PHP OUTLINE Mode buffers.")

(defvar t3php-mode-hook nil
  "Hook called by `t3php-mode'.")

(defvar t3php-highlight-overlays [nil nil]
  "A vector of different overlay to do highlighting.
This vector concerns only highlighting of horizontal lines.")

(defvar t3php-outline-mode-map ()
  "Key map used in T3PHP OUTLINE Mode buffers.")

(defvar t3php-outline-last-file nil
  "Stores the file name from which `t3php-outline' was called.")

(defvar t3php-outline-return-marker (make-marker)
  "Marker to return from outline to old position.")

(defvar t3php-outline-last-window-height nil
  "Height of *t3php-outline* window.")

(defvar t3php-outline-last-window-width nil
  "Width of *t3php-outline* window.")

(defvar t3php-outline-hl-line-overlay nil
  "Overlay used by hl-line mode to highlight the current line.")

(defvar t3php-outline-marker-list nil
  "Keeps track of all markers in PHP buffer.")

(defvar t3php-outline-last-follow-point nil
  "Remembers last follow point in *t3php-outline* buffer.")

(defvar t3php-mode-abbrev-table nil
  "Abbrev table used while in t3php mode.")
(define-abbrev-table 't3php-mode-abbrev-table ())


;;;; Functions

;;;###autoload
(defun t3php-mode ()
  "Major mode for editing TYPO3 PHP files.
Bug reports, suggestions for new features and critics should go to
`joachim <underscore> mathes <at> web <dot> de'.

This mode knows about syntax highlighting and code creation.

COMMANDS
\\{t3php-mode-map}
VARIABLES

t3php-newline-function\t\tbehaviour after pressing `RET'"
  (interactive)
  ;; Set up local variables
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'defun-prompt-regexp)
  (make-local-variable 'beginning-of-defun-function)
  (make-local-variable 'end-of-defun-function)
  (make-local-variable 'open-paren-in-column-0-is-defun-start)
  (make-local-variable 'show-trailing-whitespace)
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'show-paren-mode)

  (when (not t3php-mode-syntax-table)
    (setq t3php-mode-syntax-table (make-syntax-table))
    ;; Parenthesis
    (modify-syntax-entry ?\( "()" t3php-mode-syntax-table)
    (modify-syntax-entry ?\) ")(" t3php-mode-syntax-table)
    (modify-syntax-entry ?\[ "(]" t3php-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" t3php-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" t3php-mode-syntax-table)
    (modify-syntax-entry ?\} "){" t3php-mode-syntax-table)
    ;; String quote characters
    (modify-syntax-entry ?\' "\"" t3php-mode-syntax-table)
    ;; Comment delimiters
    (modify-syntax-entry ?/  ". 124b" t3php-mode-syntax-table)
    (modify-syntax-entry ?*  ". 23"   t3php-mode-syntax-table)
    (modify-syntax-entry ?#  "< b"    t3php-mode-syntax-table)
    (modify-syntax-entry ?\n "> b"    t3php-mode-syntax-table)
    (modify-syntax-entry ?\_ "w"      t3php-mode-syntax-table))

  (set-syntax-table t3php-mode-syntax-table)

  (setq defun-prompt-regexp "^\\s-*\\(?:\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function\\|class\\).*")
  (setq open-paren-in-column-0-is-defun-start 0)

  (if t3php-mode-map
      nil
    (setq t3php-mode-map (make-sparse-keymap))
    (define-key t3php-mode-map "\r"        't3php-newline)
    (define-key t3php-mode-map "}"         't3php-electric-brace)
    (define-key t3php-mode-map ")"         't3php-electric-brace)
    (define-key t3php-mode-map "\C-ct"     't3php-outline)
    (define-key t3php-mode-map "\C-c\C-ic" 't3php-insert-class)
    (define-key t3php-mode-map "\C-c\C-if" 't3php-insert-method)
    (define-key t3php-mode-map "\C-c\C-id" 't3php-insert-current-date)
    (define-key t3php-mode-map "\C-cd"     't3php-search-documentation)
    (define-key t3php-mode-map "\C-cw"     't3php-browse-manual)
    (define-key t3php-mode-map "\C-c\C-tw" 't3php-toggle-trailing-whitespace-visibilty))
  (use-local-map t3php-mode-map)

  (setq major-mode 't3php-mode
        mode-name "T3PHP"
        font-lock-defaults '(t3php-font-lock-keywords-3)
        comment-start "// "
        comment-end ""
        comment-start-skip "// "
        indent-line-function 't3php-indent-line
	indent-tabs-mode t
        tab-width 4 
	show-trailing-whitespace f
        parse-sexp-ignore-comments t
        show-paren-mode t)

  ;; Initialize the overlays for highlighting horizontal lines.
  (dotimes (index (length t3php-highlight-overlays))
    (when (not (aref t3php-highlight-overlays index))
      (aset t3php-highlight-overlays index (make-overlay 1 1))
      (overlay-put (aref t3php-highlight-overlays index)
                   'category 't3php-outline-hl)
      (overlay-put (aref t3php-highlight-overlays index)
                   'font-lock-face `(:background ,t3php-outline-hl-line-color))))

  ;; Run abbrev mode
  (setq local-abbrev-table t3php-mode-abbrev-table)
  (abbrev-mode t)

  ;; Run the mode hook.
  (if t3php-mode-hook
      (run-hooks 't3php-mode-hook)))

(defun t3php-insert-class ()
  "Insert class into empty php buffer.
This function inserts:
* php tags
* copyright with current date and t3php-author
* a comment block which contains SCM version ID tag, author, date
  and leaves room for a class description
* a special comment for inserting a function index automatically
  with the TYPO3 extension `Extension Development Evaluator'
* a class header comment
* the class skeleton
* a TYPO3 XCLASS inclusion section"
  (interactive)
  (insert "<?php\n"
	  "/***************************************************************\n"
	  " *  Copyright notice\n"
	  " *\n"
	  " *  (c) "  (t3php-current-year) " " t3php-author "\n"
	  " *  All rights reserved\n"
	  " *\n"
	  " *  This script is part of the TYPO3 project. The TYPO3 project is\n"
	  " *  free software; you can redistribute it and/or modify\n"
	  " *  it under the terms of the GNU General Public License as published by\n"
	  " *  the Free Software Foundation; either version 2 of the License, or\n"
	  " *  (at your option) any later version.\n"
	  " *\n"
	  " *  The GNU General Public License can be found at\n"
	  " *  http://www.gnu.org/copyleft/gpl.html.\n"
	  " *\n"
	  " *  This script is distributed in the hope that it will be useful,\n"
	  " *  but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
	  " *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
	  " *  GNU General Public License for more details.\n"
	  " *\n"
	  " *  This copyright notice MUST APPEAR in all copies of the script!\n"
	  " ***************************************************************/\n"
	  "/**\n"
	  " *\n"
	  " *\n"
	  " * @author"
	  (t3php-return-phpdoc-alignment 3)
	  t3php-author
	  "\n"
	  " * @since"
	  (t3php-return-phpdoc-alignment 4)
	  (t3php-current-date)
	  "\n"
	  " * @version"
	  (t3php-return-phpdoc-alignment 2)
	  "$" "Id" "$\n"
	  " */\n"
	  "/**\n"
	  " * [CLASS/FUNCTION INDEX of SCRIPT]\n"
	  " *\n"
	  " * Hint: use extdeveval to insert/update function index above.\n"
	  " */\n"
	  "\n\n\n\n\n"
	  "/**\n"
	  " *\n"
	  " *\n"
	  " * @author"
	  (t3php-return-phpdoc-alignment 9)
	  t3php-author
	  "\n"
	  " * @package"
	  (t3php-return-phpdoc-alignment 8)
	  "TYPO3\n"
	  " * @subpackage"
	  (t3php-return-phpdoc-alignment 2)
	  (t3php-get-subpackage-name)
	  "\n"
	  " * @since"
	  (t3php-return-phpdoc-alignment 10)
	  (t3php-current-date)
	  "\n"
	  " */\n"
	  "class "
	  (t3php-get-class-name)
	  " {\n\n"
	  "}\n\n\n"
	  "/*******************************************************************************\n"
	  " * TYPO3 XCLASS INCLUSION (for class extension/overriding)\n"
	  " ******************************************************************************/\n"
	  "if (defined('TYPO3_MODE') && $TYPO3_CONF_VARS[TYPO3_MODE]['XCLASS']['"
	  (t3php-path-to-extension-file)
	  "']) {\n"
	  "    include_once($TYPO3_CONF_VARS[TYPO3_MODE]['XCLASS']['"
	  (t3php-path-to-extension-file)
	  "']);\n"
	  "}\n"
	  "?>"))

(defun t3php-get-subpackage-name ()
  "Return subpackage name derived from extension path."
  (if (not (string-match-p
            (concat
             t3php-path-to-typo3-extension-directory
             t3php-typo3-extension-directory
	     ".*?/")
	    buffer-file-name))
      "<SUBPACKAGE>"
    (string-match (concat t3php-path-to-typo3-extension-directory
			  t3php-typo3-extension-directory
                          "\\(.*?\\)/")
                  (file-name-directory buffer-file-name))
    (match-string 1 (file-name-directory buffer-file-name))
    ))

(defun t3php-get-class-name ()
  "Return class name derived from buffer file name."
  (if (string-match-p
	    "^class."
	    (file-name-nondirectory buffer-file-name))
      (substring (file-name-sans-extension (file-name-nondirectory buffer-file-name)) 6 nil)
    (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

(defun t3php-path-to-extension-file ()
  "Return path to extension file."
  (if (not (string-match-p
            (concat
             t3php-path-to-typo3-extension-directory
             t3php-typo3-extension-directory) buffer-file-name))
      (concat "<PATH-TO-FILE>/"
              (file-name-nondirectory buffer-file-name))
    (string-match (concat t3php-path-to-typo3-extension-directory
                          "\\("
                          t3php-typo3-extension-directory
                          ".*\\)")
                  (file-name-directory buffer-file-name))
    (concat (match-string 1 (file-name-directory buffer-file-name))
            (file-name-nondirectory buffer-file-name))))

(defun t3php-insert-method (method-name)
  "Insert signature and header comment for METHOD-NAME."
  (interactive "sMethod name: ")
  (let ((method-argument)
	(method-arguments (list))
	(argument-position 0)
	(start-point (point))
	method-modifier)
    (setq method-modifier (t3php-read-method-modifier))
    (while (not (string= (setq method-argument (call-interactively 't3php-read-method-arguments)) ""))
      (push method-argument method-arguments))
    (setq method-arguments (reverse method-arguments))
    (insert (concat "/**\n"
		    "*\n*\n"))
    (if (not (eq method-arguments nil))
	(dolist (argument method-arguments)
	  (insert "* @param"
		  (t3php-return-phpdoc-alignment 3)
		  "$"
		  argument
		  "\n"))
      (insert "* @param"
	      (t3php-return-phpdoc-alignment 3)
	      "void\n"))
    (insert "* @return\n"
	    "* @author"
	    (t3php-return-phpdoc-alignment 2)
	    t3php-author
	    "\n"
	    "* @since"
	    (t3php-return-phpdoc-alignment 3)
	    (t3php-current-date)
	    "\n"
	    "*/\n"
	    )
    (insert
     (if (not (eq method-modifier nil))
                (concat method-modifier " "))
            "function "
            method-name
            "(")
    (dolist (argument method-arguments)
      (insert "$" argument)
      (if (< argument-position (1- (length method-arguments)))
          (insert ", "))
      (setq argument-position (1+ argument-position)))
    (insert ") {\n\n}")
    (indent-region start-point (point))
    (previous-line)
    (beginning-of-line)
    (indent-for-tab-command)))

(defun t3php-read-method-modifier ()
  "Read METHOD-MODIFIER from minibuffer."
  (let ((method-modifier (completing-read "Modifier: "
                                             '(("public" 1)
                                               ("private" 2)
                                               ("protected" 3))
                                             nil
					     nil
					     nil
					     nil
					     nil)))
    method-modifier))

(defun t3php-read-method-arguments (method-argument)
  "Read METHOD-ARGUMENT from minibuffer."
  (interactive "sParameter: ")
  method-argument)

(defun t3php-insert-current-date ()
  "Insert the current date into buffer."
  (interactive)
  (insert (t3php-current-date)))

 (defun t3php-current-date ()
   "Return the current date.
Uses customizable `t3php-date-format' for formatting the date."
   (format-time-string t3php-date-format (current-time)))

(defun t3php-insert-current-year ()
  "Insert the current date into buffer."
  (interactive)
  (insert (format-time-string t3php-year-format (current-time))))

(defun t3php-current-year ()
   "Return the current year.
Uses customizable `t3php-year-format' for formatting the year."
   (format-time-string t3php-year-format (current-time)))

(defun t3php-toggle-trailing-whitespace-visibilty ()
  "Toggle visbility of trailing whitespaces."
  (interactive)
  (if (eq show-trailing-whitespace nil)
      (setq show-trailing-whitespace t)
    (setq show-trailing-whitespace nil))
  (redisplay t))

(defun t3php-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX.
The region is described by the delimiters BEGIN and END.  If no optional BUFFER
provided the current buffer is used.  Highlighting is handled with
overlays.  Different indices map to different overlays."
  (move-overlay (aref t3php-highlight-overlays index)
		begin end (or buffer (current-buffer))))

(defun t3php-unhighlight (index)
  "Detach overlay INDEX."
  (delete-overlay (aref t3php-highlight-overlays index)))

(defun t3php-highlight-shall-vanish ()
  "Function used in pre-command-hook to remove highlights."
  ;; When a selected line in the t3php buffer is highlighted by a choice in the
  ;; *t3php-outline* buffer and the user switches from *t3php-outline* buffer to
  ;; t3php buffer afterwards, the selected line keeps highlighted. This is not
  ;; supposed to be like that. The highlighting should be removed. So we add
  ;; this hook to the t3php buffer `pre-command-hook' before we leave it and
  ;; remove the hook when we return while unhighlighting highlighted lines. This
  ;; hook removes itself.
  (remove-hook 'pre-command-hook 't3php-highlight-shall-vanish)
  (t3php-unhighlight 1))

(defun t3php-indent-line ()
  "Indent current line."
  (let ((cp (point))                ; current point
	(cc (current-column))       ; current column
	(ci (current-indentation))  ; current indentation
	(cl (line-number-at-pos))   ; current line
	(counter 0)
	ps                          ; parser state
	psp			    ; parser state position
	save-indent-column)

    ;; Evaluate parser state
    (save-excursion
      (beginning-of-line)
      (setq ps (t3php-parser-state))

      (cond
       ;; Check if parser state position is:
       ;; -> Inside a comment
       ((nth 8 ps)
	(setq psp (nth 8 ps))
	(goto-char psp)
	(setq save-indent-column (+ (current-column)
				    1)))
       ;; Check if parser state position is:
       ;; -> Inside a parenthetical grouping
       ((nth 1 ps)
	(setq psp (nth 1 ps))
	(cond
	 ;; Check if point is looking at a string and a closing curly brace
	 ((looking-at "[ \t[:alnum:]]*[)}]")
	  (goto-char psp)
	  (back-to-indentation)
	  (setq save-indent-column (current-column)))
	 ;; Check if previous non empty line is a `case' line
	 ((t3php-look-for-case-line)
	  (goto-char psp)
	  (back-to-indentation)
	  (setq save-indent-column (+ (current-column)
				      (* 2 t3php-block-indentation))))
	 (t
	  (goto-char psp)
	  (back-to-indentation)
	  (setq save-indent-column (+ (current-column)
				      t3php-block-indentation)))))
       ;; Check if parser state position is:
       ;; -> nil
       (t
       	;; Skip empty lines
       	(forward-line -1)
       	(while (and (looking-at "^[ \t]*\n")
       		    (not (bobp)))
       	  (forward-line -1))
       	(back-to-indentation)
	(setq save-indent-column (current-column)))))

    ;; Set indentation value on current line
    (back-to-indentation)
    (backward-delete-char-untabify (current-column))
    (indent-to save-indent-column)
    (if (> cc ci)
	(forward-char (- cc ci)))))

(defun t3php-look-for-case-line ()
  "Check if previous non empty line is a `case' line."
  (save-excursion
    (forward-line -1)
    (while (and (looking-at "^[ \t]*\n")
		(not (bobp)))
      (forward-line -1))
    (beginning-of-line)
    (looking-at "^\\s-*case")))

(defun t3php-parser-state ()
  "Return the parser state at point."
  (save-excursion
    (let ((here (point))
	  sps)
      ;; For correct indentation the character position of the start of the
      ;; innermost parenthetical grouping has to be found.
      (goto-char (point-min))
      ;; Now get the parser state, i.e. the depth in parentheses.
      (save-excursion
	(setq sps (parse-partial-sexp (point) here)))
      sps)))

(defun t3php-electric-brace (arg)
  "Insert closing brace.

Argument ARG prefix."
  (interactive "*P")
  ;; Insert closing brace.
  (self-insert-command (prefix-numeric-value arg))

  (when (and (looking-at "[ \t]*$")
	     (looking-back "^[ \t]*[})]"))
    (t3php-indent-line)))

(defun t3php-newline ()
  "Call the dedicated newline function.

The variable `t3php-newline-function' decides which newline function to
use."
  (interactive)
  (funcall t3php-newline-function))

(defun t3php-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat t3php-php-search-url (current-word t))))

(defun t3php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url t3php-php-manual-url))

(defun t3php-return-phpdoc-alignment (number-of-spaces)
  "Returns NUMBER-OF-SPACES as PHPDoc aligment if t3php-phpdoc-align is not nil."
  (if t3php-phpdoc-align
      (make-string number-of-spaces ?\s)
    (make-string 1 ?\s)))

(defun t3php-outline-mode ()
  "Major mode for managing the outline of php files.

COMMANDS
\\{t3php-outline-mode-map}
VARIABLES

t3php-outline-keep-other-windows\t\twindow configuration when outline window is viewed
t3php-outline-split-windows-horizontally\tdirection of window splitting
t3php-outline-split-windows-fraction\toutline window size
t3php-outline-follow-mode\t\t\ttoggle follow mode
t3php-outline-hl-line-color\t\tcolor of highlighted horizontal line
t3php-outline-block-name-color\t\tcolor used to highlight block names"
  (interactive)
  ;; Set up local variables
  (kill-all-local-variables)
  (make-local-variable 'truncate-lines)
  (make-local-variable 'show-paren-mode)

  (setq major-mode      't3php-outline-mode
        mode-name       "T3PHP-OUTLINE"
        truncate-lines  t
        show-paren-mode nil)

  (add-hook 'pre-command-hook
	    't3php-outline-pre-command-hook nil t)
  (add-hook 'post-command-hook
	    't3php-outline-post-command-hook nil t)

  (if t3php-outline-mode-map
      nil
    (setq t3php-outline-mode-map (make-sparse-keymap))
    (substitute-key-definition
     'forward-char 't3php-outline-next t3php-outline-mode-map global-map)
    (substitute-key-definition
     'backward-char 't3php-outline-previous t3php-outline-mode-map global-map)
    (substitute-key-definition
     'next-line 't3php-outline-next t3php-outline-mode-map global-map)
    (substitute-key-definition
     'previous-line 't3php-outline-previous t3php-outline-mode-map global-map)
    (substitute-key-definition
     'beginning-of-buffer 't3php-outline-beginning-of-buffer t3php-outline-mode-map global-map)
    (substitute-key-definition
     'end-of-buffer 't3php-outline-end-of-buffer t3php-outline-mode-map global-map)
    (define-key t3php-outline-mode-map "?"    't3php-outline-show-help)
    (define-key t3php-outline-mode-map "n"    't3php-outline-next)
    (define-key t3php-outline-mode-map "p"    't3php-outline-previous)
    (define-key t3php-outline-mode-map " "    't3php-outline-view-line)
    (define-key t3php-outline-mode-map "\C-i" 't3php-outline-goto-line)
    (define-key t3php-outline-mode-map "\C-m" 't3php-outline-goto-line-and-kill)
    (define-key t3php-outline-mode-map "f"    't3php-outline-toggle-follow)
    (define-key t3php-outline-mode-map "r"    't3php-outline-rescan)
    (define-key t3php-outline-mode-map "q"    't3php-outline-quit))
  (use-local-map t3php-outline-mode-map))

(defun t3php-outline (&optional rescan)
  "Show the outline for the current t3php buffer.

The outline consists of the methods in the current t3php buffer.  If
RESCAN is true, rescan the t3php buffer before displaying the *t3php-outline*
buffer."
  (interactive)

  (if (or (not (string= t3php-outline-last-file (buffer-file-name)))
	  rescan)
      (t3php-outline-erase-outline-buffer))

  (setq t3php-outline-last-file (buffer-file-name))

  (set-marker t3php-outline-return-marker (point))

  ;; If follow mode is active, prepare to delay it one command. This prevents
  ;; follow mode from being executed when this command `t3php-outline' is called.
  (when t3php-outline-follow-mode
    (setq t3php-outline-follow-mode 1))

  ;; Set the window configuration
  (let ((unsplittable (frame-parameter (selected-frame) 'unsplittable))
	outline-window foobar)
    ;; Select outline buffer window
    (if (setq outline-window (get-buffer-window t3php-outline-buffer-name))
	(select-window outline-window)
      (when (or (not t3php-outline-keep-other-windows)
		(< (window-height) (* 2 window-min-height)))
	(delete-other-windows))

      ;; Remember size of window
      (setq t3php-outline-last-window-width (window-width)
	    t3php-outline-last-window-height (window-height))

      ;; Split window
      (unless unsplittable
	(if t3php-outline-split-windows-horizontally
	    (split-window-horizontally
	     (floor (* (window-width)
		       t3php-outline-split-windows-fraction)))
	  (split-window-vertically
	   (floor (* (window-height)
		     t3php-outline-split-windows-fraction)))))

      ;; Set major mode for and switch to *t3php-outline* buffer
      (let ((default-major-mode 't3php-outline-mode))
	(switch-to-buffer t3php-outline-buffer-name)))

    (or (eq major-mode 't3php-outline-mode) (t3php-outline-mode))

    (cond
     ((= (buffer-size) 0)
     ;; If buffer is empty, fill it with the outline
      (message "Building *t3php-outline* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"OUTLINE of %s
SPC=view TAB=goto RET=goto+kill [f]ollow [r]escan  [k]ill [?]Help
-----------------------------------------------------------------------
" (abbreviate-file-name t3php-outline-last-file)))

      ;; Set text properties of *t3php-outline* buffer header
      (put-text-property (point-min) (point) 'font-lock-face font-lock-comment-face)
      (put-text-property (point-min) (point) 'intangible t)
      ;; Fill the buffer with the outline
      (dolist (line
	       (t3php-outline-format
		(t3php-outline-content
		 (get-file-buffer t3php-outline-last-file))))
	(insert line))
      (goto-line 4)
      (beginning-of-line)
      (setq buffer-read-only t)
      (message "Building *t3php-outline* buffer...done."))
     (t
      ;; Only set offset
      (goto-line 4)
      (beginning-of-line)))))

(defun t3php-outline-erase-outline-buffer ()
  "Erase T3PHP OUTLINE buffer, if it exists."
  (if (get-buffer t3php-outline-buffer-name)
      (save-excursion
	(set-buffer t3php-outline-buffer-name)
	(let ((buffer-read-only nil))
	  (erase-buffer)))))

(defun t3php-outline-post-command-hook ()
  "Used in `post-command-hook' for *t3php-outline* buffer.

Activates the hl-line overlay on the current line.
Handles follow mode if activated."
  (when (get-buffer t3php-outline-buffer-name)
    (t3php-highlight 0
		     (line-beginning-position)
		     (line-beginning-position 2)
		     (get-buffer t3php-outline-buffer-name)))
  (if (integerp t3php-outline-follow-mode)
      ;; Remove delayed follow action.
      (setq t3php-outline-follow-mode t)
    (when (and t3php-outline-follow-mode
	       (not (equal t3php-outline-last-follow-point (point))))
      (setq t3php-outline-last-follow-point (point))
      (t3php-outline-visit-location 'view))))

(defun t3php-outline-pre-command-hook ()
  "Used in `pre-command-hook' for *t3php-outline* buffer.

Deactivates the hl-line overlay on the current line."
  (when (get-buffer t3php-outline-buffer-name)
    (t3php-unhighlight 0)))

(defun t3php-re-enlarge ()
  "Enlarge t3php window to a remembered size."
  (if t3php-outline-split-windows-horizontally
      (enlarge-window-horizontally
       (max 0 (- (or t3php-outline-last-window-width (window-width))
		 (window-width))))
    (enlarge-window
     (max 0 (- (or t3php-outline-last-window-height (window-height))
	       (window-height))))))

(defun t3php-outline-show-help ()
  "Show a summary of key bindings."
  (interactive)
  (with-output-to-temp-buffer "*T3php OUTLINE Help*"
    (princ t3php-outline-help))
  ;; t3php-enlarge-to-fit
  )

(defun t3php-outline-next (&optional arg)
  "Move to next selectable item.

Up to now this is just the next line. There might be different kinds of contents
in the outline in the future. It could be usefull to find the next meaningfull
line by text properties."
  (interactive "p")
  (forward-line arg))

(defun t3php-outline-previous (&optional arg)
  "Move to previous selectable item.

Up to now this is just the next line. There might be different kinds of contents
in the outline in the future. It could be usefull to find the next meaningfull
line by text properties."
  (interactive "p")

  (if (not arg)
      (setq arg 1))
  (if (<= (- (line-number-at-pos (point)) arg) 3)
      (progn
      (goto-line 4)
      (recenter))
    (forward-line (* -1 arg))))

(defun t3php-outline-view-line ()
  "Show the corresponding location in the t3php buffer."
  (interactive)
  (t3php-outline-visit-location 'view))

(defun t3php-outline-goto-line ()
  "Go to the location and keep the *t3php-outline* window."
  (interactive)
  (t3php-outline-visit-location 'goto))

(defun t3php-outline-goto-line-and-kill ()
  "Go to the location and kill the *t3php-outline* window."
  (interactive)
  (t3php-outline-visit-location 'goto-and-kill))

(defun t3php-outline-toggle-follow ()
  "Toggle follow mode in *t3php-outline* buffer."
  (interactive)
  (setq t3php-outline-last-follow-point -1)
  (setq t3php-outline-follow-mode (not t3php-outline-follow-mode)))

(defun t3php-outline-rescan ()
  "Regenerate *t3php-outline* buffer by reparsing the t3php buffer."
  (interactive)
  (switch-to-buffer-other-window (get-file-buffer t3php-outline-last-file))
  (t3php-outline t))

(defun t3php-outline-quit ()
  "Quit (kill) the *t3php-outline* buffer."
  (interactive)
  (kill-buffer t3php-outline-buffer-name)
  (unless (one-window-p)
    (delete-window))
  (if (eq nil (marker-position t3php-outline-return-marker))
      (progn (switch-to-buffer nil)
	     (message "No associated buffer found."))
    (switch-to-buffer (marker-buffer t3php-outline-return-marker))
    (t3php-re-enlarge)
    (goto-char (marker-position t3php-outline-return-marker))))

(defun t3php-outline-beginning-of-buffer ()
  "Go to the beginning of the *t3php-outline* buffer."
  (interactive)
  (goto-line 4)
  (recenter))

(defun t3php-outline-end-of-buffer ()
  "Go to the end of the *t3php-outline* buffer."
  (interactive)
  (goto-line (line-number-at-pos (point-max))))

(defun t3php-outline-content (t3php-buffer)
  "Return outline of methods.
This is a list of lists containing the method modifier, method name,
method start and end position."
  (save-current-buffer
    ;; Processing a buffer with lots of markers slows processing, like inserting
    ;; or deleting text, in a buffer. Unreferenced markers are garbage collected
    ;; eventually, but until then will continue to use time if they do point
    ;; somewehere. Therefore it is helpfull to make a marker point nowhere, when
    ;; it is not used anymore. Thus clear all markers in the list
    ;; `t3php-outline-marker-list', which keeps track of all markers.
    (when t3php-outline-marker-list
      (dolist (t3php-outline-marker t3php-outline-marker-list)
	(set-marker t3php-outline-marker nil))
      (setq t3php-outline-marker-list (list)))

    ;; Switch to t3php buffer.
    (set-buffer t3php-buffer)

    (let ((start (point-min))
	  (end (point-max))
	  (method-end (point-min))
	  tbs
	  list-of-methods)
      (save-excursion
	(goto-char start)

	(catch 'no-valid-method
	  (while t
	    (let ((method-marker (make-marker))
		  (method-start
		   ;; Look for method method start
		   (save-excursion
		     (goto-char method-end)
		     (setq tbs (re-search-forward
				"^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function.*" nil t))
		     ;; If search was successfull set method-start to the beginning
		     ;; of the line; return nil otherwise
		     (if (not tbs)
			 nil
		       (goto-char tbs)
		       (beginning-of-line)
		       (point)))))
	      (if method-start
		  (let (method-name
			method-modifier)
			 ;; Save method name
		    (save-excursion
		      (goto-char method-start)
		      (looking-at
		       (concat
			"^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function\\s-+&?"
			"\\(\\w+?\\)\\s-*("))
		      (setq method-name (match-string 2)
			    method-modifier (match-string 1)))
		    ;; Look for measurment method end
		    (save-excursion
		      (goto-char method-start)
		      (end-of-defun)
		      (setq method-end (point)))
		    ;; The following local variables are defined up to here:
		    ;; [1] method-start   : point of measurement method start
		    ;; [2] method-end     : point of method end
		    ;; [3] method-name    : name of method
		    ;; [4] method-modifier: name of method modifier
		    (if (and (<= method-start end)
			     (<= method-end end))
			(progn
			  (set-marker method-marker method-start)
			  (push method-marker t3php-outline-marker-list)
			  (push (list
				 (line-number-at-pos method-start)  ; method start
				 (line-number-at-pos method-end)    ; method end
				 method-name                        ; method name
				 method-marker                      ; method marker
				 method-modifier)                   ; method modifier
				list-of-methods))
		      (throw 'no-valid-method t)))
		(throw 'no-valid-method t))))))
      (reverse list-of-methods))))

(defun t3php-outline-format (content)
  "Return a list of formatted lines for the outline.
CONTENT is the list of lists returned by function `t3php-outline-content'."
  (let* ((formatting-information (t3php-outline-formatting-information content))
	 (formatted-content (list))
	 (max-method-name-length (nth 0 formatting-information))
	 (max-method-inf-from-length (nth 1 formatting-information))
	 (max-method-inf-to-length (nth 2 formatting-information))
	 (max-method-inf-length (nth 3 formatting-information))
	 method-start
	 method-end
	 method-name
	 method-marker
	 method-modifier
	 symbolized-method-modifier
	 (formatter (concat "%s "
			    "%-"
			    (number-to-string (+ max-method-name-length 6))
			    "s"
			    "[ %"
			    (number-to-string max-method-inf-from-length)
			    "d-%-"
			    (number-to-string max-method-inf-to-length)
			    "d ]\n"))
	 formatted-line)
    (dolist (line content)
      (setq method-start (nth 0 line))
      (setq method-end (nth 1 line))
      (setq method-name (nth 2 line))
      (setq method-marker (nth 3 line))
      (setq method-modifier (nth 4 line))

      ;; Set text properties for method-modifier
      (cond ((string= "private" method-modifier)
	     (setq symbolized-method-modifier "[-]")
	     (put-text-property 0 (length symbolized-method-modifier)
				'font-lock-face `(:foreground
						  "firebrick")
				symbolized-method-modifier))
	    ((string= "protected" method-modifier)
	     (setq symbolized-method-modifier "[#]")
	     (put-text-property 0 (length symbolized-method-modifier)
				'font-lock-face `(:foreground
						  "goldenrod")
				symbolized-method-modifier))
	    ((string= "public" method-modifier)
	     (setq symbolized-method-modifier "[+]")
	     (put-text-property 0 (length symbolized-method-modifier)
				'font-lock-face `(:foreground
						  "forest green")
				symbolized-method-modifier))
	    ((eq nil method-modifier)
	     (setq symbolized-method-modifier "[+]")
	     (put-text-property 0 (length symbolized-method-modifier)
				'font-lock-face `(:foreground
						  "forest green")
				symbolized-method-modifier)))

      ;; Set text properties for method-name
      (remove-text-properties 0 (length method-name)
			 '(face nil) method-name)
      (put-text-property 0 (length method-name)
			 'font-lock-face `(:foreground
					   ,t3php-outline-method-name-color)
			 method-name)
      (put-text-property 0 (length method-name)
			 'help-echo method-name
			 method-name)
      (put-text-property 0 (length method-name)
			 'fontified t
			 method-name)

      (setq formatted-line
	    (if (> (length method-name) max-method-name-length)
		(format formatter symbolized-method-modifier
			(concat (substring method-name
					   0
					   max-method-name-length)
				"...")
			method-start method-end)
	      (format formatter symbolized-method-modifier method-name method-start method-end)))
      ;; The asterisk `*' holds the method information like `method-name' and
      ;; `method-marker' as a text property.
      (put-text-property 0 1 'data `(,method-marker ,method-name) formatted-line)

      (push formatted-line formatted-content))

    ;; Remove last newline character of last element in `formatted-content'.
    (unless (null formatted-content)
      (push (substring (pop formatted-content) 0 -1) formatted-content))

    (reverse formatted-content)))

(defun t3php-outline-formatting-information (content)
  "Return a list of formatting information for the outline.
The first item is the maximum string length allowed for method
names.  The value is limited by the current window width with respect to the
length of a string, which shows the measurement block start and end lines
aligned to the right border of the window.  The string sizes of the latter values
are also evaluated and provide the second and third item of the information
list.  The fourth item is made of their sum plus an aditional value.  Thus we have
a list like this: (max-block-name-length max-block-inf-from-size
max-block-inf-to-size max-block-inf-size).  CONTENT is the list of lists returned
by function `t3php-outline-content'"
  (let ((max-from 0)
	(max-to 0)
	(formatting-information (list)))
      (dolist (line content)
	(setq max-from (max max-from (nth 0 line)))
	(setq max-to (max max-to (nth 1 line))))
      ;; The block size information string shall look like this:
      ;; [ 1234-5678 ]
      ;; Thus evaluate the string length with respect to this.
      (push (+ (length (number-to-string max-from))
	       (length (number-to-string max-to))
	       5)
	    formatting-information) ; number of characters `[ - ]' = 5

      (push (length (number-to-string max-to))
	    formatting-information)

      (push (length (number-to-string max-from))
	    formatting-information)

      ;; The maximum block name length shall be 20.
      (push 40 formatting-information)
      formatting-information))

(defun t3php-outline-visit-location (visit-mode)
  "Visit t3php buffer according to VISIT-MODE."
  (let* ((outline-data (get-text-property (point) 'data))
	 (outline-window (selected-window))
	 (t3php-marker (nth 0 outline-data))
	 (t3php-block-name (nth 1 outline-data))
	 show-window show-buffer
	 match)

    (unless outline-data (message "%s" "Don't know which outline line to visit."))

    (setq match
	  (cond
	   ((and (markerp t3php-marker) (marker-buffer t3php-marker))
	    ;; Marker is available and buffer still exists.
	    (switch-to-buffer-other-window (marker-buffer t3php-marker))
	    (goto-char (marker-position t3php-marker))
	    (looking-at (concat
			 "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function\\s-+&?"
			 t3php-block-name)))
	   (t
	    ;; Marker is lost. A backup method might be implemented in the
	    ;; future. For now just print an error message.
	    (message "Marker is lost."))))

    (when match
      (goto-char (match-beginning 0))
      (t3php-highlight 1 (match-beginning 0) (match-end 0) (current-buffer))
      (add-hook 'pre-command-hook 't3php-highlight-shall-vanish))

    (setq show-window (selected-window)
	  show-buffer (current-buffer))

    (if (not match)
	(progn
	  (select-window outline-window)
	  (message "Cannot find location."))

      ;; VISIT-MODE decides what to do next.
      (cond
       ((eq visit-mode 'view)
	(select-window outline-window)
	(recenter))
       ((eq visit-mode 'goto)
	(t3php-unhighlight 1)
	(select-window show-window)
	(recenter))
       ((eq visit-mode 'goto-and-kill)
	(select-window outline-window)
	(t3php-unhighlight 1)
	(kill-buffer t3php-outline-buffer-name)
        (unless (one-window-p)
	  (delete-window))
	(if (window-live-p show-window)
	    (set-buffer show-buffer)
	  (switch-to-buffer show-buffer))
	(t3php-re-enlarge))
       (t nil)))))

(provide 't3php-mode)

;;; t3php-mode.el ends here
