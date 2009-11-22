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

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(defconst t3php-version "0.0.1"
  "`t3php-mode' version number.")

;;;; User definable variables

(defgroup typo3php nil
  "Major mode for editing TYPO3 PHP files."
  :prefix "t3php-"
  :group 'languages)

(defcustom t3php-default-face 'default
  "Default face in `t3php-mode' buffers."
  :type 'face
  :group 't3php)

(defcustom t3php-developer "Lisa Fremont <lisa@fremont.de>"
  "Developer for php-doc."
  :type 'string
  :group 't3php)

(defcustom t3php-date-format "%Y-%m-%d"
  "Date format for php-doc.
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

(defcustom t3php-php-manual-url "http://www.php.net/manual/en/"
  "URL at which to find PHP manual.
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 't3php)

(defcustom t3php-php-search-url "http://www.php.net/"
  "URL at which to search for documentation on a word."
  :type 'string
  :group 't3php)

(defcustom t3php-toc-keep-other-windows t
  "If true, split the selected window to display the *t3php-toc* buffer.

When nil, all other windows except the selected one will be deleted, so that the
*t3php-toc* window fills half the frame."
  :type 'boolean
  :group 't3php)

(defcustom t3php-toc-split-windows-horizontally nil
  "If true, create toc windows by splitting horizontally.
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

(defcustom t3php-toc-split-windows-fraction .3
  "Fraction of the width or height of the window to be used for toc window."
  :type 'number
  :group 't3php)

(defcustom t3php-toc-follow-mode nil
  "If true, point in *t3php-toc* will make PHP window to follow.

It will show the corresponding part of the document.  This flag can be toggled
from within the *t3php-toc* buffer with the `f' key."
  :type 'boolean
  :group 't3php)

(defcustom t3php-toc-hl-line-color "DarkSlateBlue"
  "The color used to highlight the horizontal line.

  The default value is `DarkSlateBlue'. For a list of all available colors use `M-x
list-colors-display"
  :type 'color
  :group 't3php)

(defcustom t3php-toc-block-name-color "sea green"
  "The color used to highlight function names in the TOC.

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
    '("->\\(\\sw+\\)\\s-*(" . (1 t3php-default-face t t)) ;; ->function_call
    '("\\(\\sw+\\)::\\sw+\\s-*(?" . (1 font-lock-type-face)) ;; class::member
    '("::\\(\\sw+\\>[^(]\\)" . (1 t3php-default-face)) ;; class::constant
    '("\\<\\sw+\\s-*[[(]" . t3php-default-face) ;; word( or word[
    '("\\<[0-9]+" . t3php-default-face) ;; number (also matches word)

    ;; Exclude casts from bare-word treatment (may contain spaces)
    `(,(concat "(\\s-*\\(" t3php-types "\\)\\s-*)")
      1 font-lock-type-face)

    ;; PHP5: function declarations may contain classes as parameters type
    `(,(concat "[(,]\\s-*\\(\\sw+\\)\\s-+&?\\$\\sw+\\>")
      1 font-lock-type-face)

    ;; Warn about '$' immediately after ->
    '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
      (1 font-lock-warning-face) (2 t3php-default-face))

    ;; Warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)

    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)

    ;; Warn on any words not already fontified
    '("\\<\\sw+\\>" . font-lock-warning-face)))
  "High level highlighting for T3PHP mode.")

(defconst t3php-toc-buffer-name "*t3php-toc*"
  "Name of t3php toc buffer.")

(defconst t3php-toc-help
"Available keys in T3PHP TOC buffer:
=====================================
n/<right>/<down> Move to next selectable item.
p/<left>/<up>    Move to previous selectable item.
<C-home>         Move to the beginning of the buffer.
<C-end>          Move too the end of the buffer.
SPC              Show the corresponding location of the TYPO3 PHP buffer.
TAB              Go to the location and keep the *t3php-toc* window.
RET              Go to the location and hide the *t3php-toc* window.
f                Toggle follow mode.
r                Reparse the PHP buffer.
q                Hide *t3php-toc* window.
k                Kill *t3php-toc* buffer.
?                Show this help side.")


;;;; Internal variables

(defvar t3php-mode-syntax-table nil
  "Syntax table used in T3PHP Mode buffers.")

(defvar t3php-mode-map ()
  "Key map used in T3PHP TOC Mode buffers.")

(defvar t3php-mode-hook nil
  "Hook called by `t3php-mode'.")

(defvar t3php-highlight-overlays [nil nil]
  "A vector of different overlay to do highlighting.
This vector concerns only highlighting of horizontal lines.")

(defvar t3php-toc-mode-map ()
  "Key map used in T3PHP TOC Mode buffers.")

(defvar t3php-toc-last-file nil
  "Stores the file name from which `t3php-toc' was called.")

(defvar t3php-toc-return-marker (make-marker)
  "Marker to return from toc to old position.")

(defvar t3php-toc-last-window-height nil
  "Height of *t3php-toc* window.")

(defvar t3php-toc-last-window-width nil
  "Width of *t3php-toc* window.")

(defvar t3php-toc-hl-line-overlay nil
  "Overlay used by hl-line mode to highlight the current line.")

(defvar t3php-toc-marker-list nil
  "Keeps track of all markers in PHP buffer.")

(defvar t3php-toc-last-follow-point nil
  "Remembers last follow point in *t3php-toc* buffer.")

(defvar t3php-mode-abbrev-table nil
  "Abbrev table used while in t3php mode.")
(define-abbrev-table 't3php-mode-abbrev-table ())


;;;; Functions

;;;###autoload
(defun t3php-mode ()
  "Major mode for editing TYPO3 php files.
Bug reports, suggestions for new features and critics should go to
`joachim_mathes@web.de'.

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
    (define-key t3php-mode-map "\C-ct"     't3php-toc)
    (define-key t3php-mode-map "\C-cf"     't3php-insert-method)
    (define-key t3php-mode-map "\C-cd"     't3php-search-documentation)
    (define-key t3php-mode-map "\C-cw"     't3php-browse-manual)
    (define-key t3php-mode-map "\C-c\C-tw" 't3php-toggle-trailing-whitespace-visibilty))
  (use-local-map t3php-mode-map)

  (setq major-mode 't3php-mode
        mode-name "TYPO3 PHP mode"
        font-lock-defaults '(t3php-font-lock-keywords-3)
        comment-start "// "
        comment-end ""
        comment-start-skip "// "
        indent-line-function 't3php-indent-line
        show-trailing-whitespace t
	parse-sexp-ignore-comments t)

  ;; Initialize the overlays for highlighting horizontal lines.
  (dotimes (index (length t3php-highlight-overlays))
    (when (not (aref t3php-highlight-overlays index))
      (aset t3php-highlight-overlays index (make-overlay 1 1))
      (overlay-put (aref t3php-highlight-overlays index)
		   'category 't3php-toc-hl)
      (overlay-put (aref t3php-highlight-overlays index)
		   'font-lock-face `(:background ,t3php-toc-hl-line-color))))

  ;; Run abbrev mode
  (setq local-abbrev-table t3php-mode-abbrev-table)
  (abbrev-mode t)

  ;; Run the mode hook.
  (if t3php-mode-hook
      (run-hooks 't3php-mode-hook)))

(defun t3php-insert-method (method-name)
  "Insert signature and header comment for METHOD-NAME."
  (interactive "sMethod name: ")
  (let ((method-argument)
	(method-arguments (list))
	(argument-position 0)
	(start-point (point)))
    (setq method-modificator (t3php-read-method-modificator))
    (while (not (string= (setq method-argument (call-interactively 't3php-read-method-arguments)) ""))
      (push method-argument method-arguments))
    (setq method-arguments (reverse method-arguments))
    (insert (concat "/**\n"
		    "*\n*\n"))
    (dolist (argument method-arguments)
      (insert (concat "* @param $"
		      argument
		      "\n")))
    (insert (concat "* @return\n"
		    "* @author "
		    t3php-developer
		    "\n"
		    "* @since "
		    (t3php-current-date)
		    "\n"
		    "*/\n"
		    ))
    (insert
     (if (not (eq method-modificator nil))
                (concat method-modificator " "))
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

(defun t3php-read-method-modificator ()
  "Read METHOD-MODIFICATOR from minibuffer."
  (let ((method-modificator (completing-read "Modificator: "
                                             '(("public" 1)
                                               ("private" 2)
                                               ("protected" 3))
                                             nil
					     nil
					     nil
					     nil
					     nil)))
    method-modificator))

(defun t3php-read-method-arguments (method-argument)
  "Read METHOD-ARGUMENT from minibuffer."
  (interactive "sParameter: ")
  method-argument)

(defun t3php-current-date ()
  "Insert the current date.
Uses `current-date-time-format' for the formatting the date/time."
  (format-time-string t3php-date-format (current-time)))

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
  ;; *t3php-toc* buffer and the user switches from *t3php-toc* buffer to
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

(defun t3php-toc-mode ()
  "Major mode for managing Table of Contents for php files.

COMMANDS
\\{t3php-toc-mode-map}
VARIABLES

t3php-toc-keep-other-windows\t\twindow configuration when toc window is viewed
t3php-toc-split-windows-horizontally\tdirection of window splitting
t3php-toc-split-windows-fraction\ttoc window size
t3php-toc-follow-mode\t\t\ttoggle follow mode
t3php-toc-hl-line-color\t\tcolor of highlighted horizontal line
t3php-toc-block-name-color\t\tcolor used to highlight block names"
  (interactive)
  ;; Set up local variables
  (kill-all-local-variables)

  (setq major-mode         't3php-toc-mode
	mode-name          "TYPO3 PHP TOC"
	truncate-lines     t)

  (add-hook 'pre-command-hook
	    't3php-toc-pre-command-hook nil t)
  (add-hook 'post-command-hook
	    't3php-toc-post-command-hook nil t)

  (if t3php-toc-mode-map
      nil
    (setq t3php-toc-mode-map (make-sparse-keymap))
    (substitute-key-definition
     'forward-char 't3php-toc-next t3php-toc-mode-map global-map)
    (substitute-key-definition
     'backward-char 't3php-toc-previous t3php-toc-mode-map global-map)
    (substitute-key-definition
     'next-line 't3php-toc-next t3php-toc-mode-map global-map)
    (substitute-key-definition
     'previous-line 't3php-toc-previous t3php-toc-mode-map global-map)
    (substitute-key-definition
     'beginning-of-buffer 't3php-toc-beginning-of-buffer t3php-toc-mode-map global-map)
    (substitute-key-definition
     'end-of-buffer 't3php-toc-end-of-buffer t3php-toc-mode-map global-map)
    (define-key t3php-toc-mode-map "?"    't3php-toc-show-help)
    (define-key t3php-toc-mode-map "n"    't3php-toc-next)
    (define-key t3php-toc-mode-map "p"    't3php-toc-previous)
    (define-key t3php-toc-mode-map " "    't3php-toc-view-line)
    (define-key t3php-toc-mode-map "\C-i" 't3php-toc-goto-line)
    (define-key t3php-toc-mode-map "\C-m" 't3php-toc-goto-line-and-hide)
    (define-key t3php-toc-mode-map "f"    't3php-toc-toggle-follow)
    (define-key t3php-toc-mode-map "r"    't3php-toc-rescan)
    (define-key t3php-toc-mode-map "q"    't3php-toc-quit)
    (define-key t3php-toc-mode-map "k"    't3php-toc-quit-and-kill))
  (use-local-map t3php-toc-mode-map))

(defun t3php-toc (&optional rescan)
  "Show the table of contents for the current t3php buffer.

The table of contents consists of the methods in the current t3php buffer.  If
RESCAN is true, rescan the t3php buffer before displaying the *t3php-toc*
buffer."
  (interactive)

  (if (or (not (string= t3php-toc-last-file (buffer-file-name)))
	  rescan)
      (t3php-toc-erase-toc-buffer))

  (setq t3php-toc-last-file (buffer-file-name))

  (set-marker t3php-toc-return-marker (point))

  ;; If follow mode is active, prepare to delay it one command. This prevents
  ;; follow mode from being executed when this command `t3php-toc' is called.
  (when t3php-toc-follow-mode
    (setq t3php-toc-follow-mode 1))

  ;; Set the window configuration
  (let ((unsplittable (frame-parameter (selected-frame) 'unsplittable))
	toc-window foobar)
    ;; Select toc buffer window
    (if (setq toc-window (get-buffer-window t3php-toc-buffer-name))
	(select-window toc-window)
      (when (or (not t3php-toc-keep-other-windows)
		(< (window-height) (* 2 window-min-height)))
	(delete-other-windows))

      ;; Remember size of window
      (setq t3php-toc-last-window-width (window-width)
	    t3php-toc-last-window-height (window-height))

      ;; Split window
      (unless unsplittable
	(if t3php-toc-split-windows-horizontally
	    (split-window-horizontally
	     (floor (* (window-width)
		       t3php-toc-split-windows-fraction)))
	  (split-window-vertically
	   (floor (* (window-height)
		     t3php-toc-split-windows-fraction)))))

      ;; Set major mode for and switch to *t3php-toc* buffer
      (let ((default-major-mode 't3php-toc-mode))
	(switch-to-buffer t3php-toc-buffer-name)))

    (or (eq major-mode 't3php-toc-mode) (t3php-toc-mode))

    (cond
     ((= (buffer-size) 0)
     ;; If buffer is empty, fill it with the table of contents
      (message "Building *t3php-toc* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"TABLE OF CONTENTS of %s
SPC=view TAB=goto RET=goto+hide [f]ollow [r]escan [q]uit [k]ill [?]Help
-----------------------------------------------------------------------
" (abbreviate-file-name t3php-toc-last-file)))

      ;; Set text properties of *t3php-toc* buffer header
      (put-text-property (point-min) (point) 'font-lock-face font-lock-comment-face)
      (put-text-property (point-min) (point) 'intangible t)
      ;; Fill the buffer with a table of methods
      (dolist (line
	       (t3php-toc-format
		(t3php-toc-content
		 (get-file-buffer t3php-toc-last-file))))
	(insert line))
      (goto-line 4)
      (beginning-of-line)
      (setq buffer-read-only t)
      (message "Building *t3php-toc* buffer...done."))
     (t
      ;; Only set offset
      (goto-line 4)
      (beginning-of-line)))))

(defun t3php-toc-erase-toc-buffer ()
  "Erase t3php toc buffer, if it exists."
  (if (get-buffer t3php-toc-buffer-name)
      (save-excursion
	(set-buffer t3php-toc-buffer-name)
	(let ((buffer-read-only nil))
	  (erase-buffer)))))

(defun t3php-toc-post-command-hook ()
  "Used in `post-command-hook' for *t3php-toc* buffer.

Activates the hl-line overlay on the current line.
Handles follow mode if activated."
  (when (get-buffer t3php-toc-buffer-name)
    (t3php-highlight 0
		     (line-beginning-position)
		     (line-beginning-position 2)
		     (get-buffer t3php-toc-buffer-name)))
  (if (integerp t3php-toc-follow-mode)
      ;; Remove delayed follow action.
      (setq t3php-toc-follow-mode t)
    (when (and t3php-toc-follow-mode
	       (not (equal t3php-toc-last-follow-point (point))))
      (setq t3php-toc-last-follow-point (point))
      (t3php-toc-visit-location 'view))))

(defun t3php-toc-pre-command-hook ()
  "Used in `pre-command-hook' for *t3php-toc* buffer.

Deactivates the hl-line overlay on the current line."
  (when (get-buffer t3php-toc-buffer-name)
    (t3php-unhighlight 0)))

(defun t3php-re-enlarge ()
  "Enlarge t3php window to a remembered size."
  (if t3php-toc-split-windows-horizontally
      (enlarge-window-horizontally
       (max 0 (- (or t3php-toc-last-window-width (window-width))
		 (window-width))))
    (enlarge-window
     (max 0 (- (or t3php-toc-last-window-height (window-height))
	       (window-height))))))

(defun t3php-toc-show-help ()
  "Show a summary of key bindings."
  (interactive)
  (with-output-to-temp-buffer "*T3php TOC Help*"
    (princ t3php-toc-help))
  ;; t3php-enlarge-to-fit
  )

(defun t3php-toc-next (&optional arg)
  "Move to next selectable item.

Up to now this is just the next line. There might be different kinds of contents
in the table of contents in the future. It could be usefull to find the next
meaningfull line by text properties."
  (interactive "p")
  (forward-line arg))

(defun t3php-toc-previous (&optional arg)
  "Move to previous selectable item.

Up to now this is just the next line. There might be different kinds of contents
in the table of contents in the future. It could be usefull to find the next
meaningfull line by text properties."
  (interactive "p")

  (if (not arg)
      (setq arg 1))
  (if (<= (- (line-number-at-pos (point)) arg) 3)
      (progn
      (goto-line 4)
      (recenter))
    (forward-line (* -1 arg))))

(defun t3php-toc-quit ()
  "Hide the *t3php-toc* window and do not move point."
  (interactive)
  (unless (one-window-p)
    (delete-window))
  (switch-to-buffer (marker-buffer t3php-toc-return-marker))
  (t3php-re-enlarge)
  (goto-char (marker-position t3php-toc-return-marker)))

(defun t3php-toc-view-line ()
  "Show the corresponding location in the t3php buffer."
  (interactive)
  (t3php-toc-visit-location 'view))

(defun t3php-toc-goto-line ()
  "Go to the location and keep the *t3php-toc* window."
  (interactive)
  (t3php-toc-visit-location 'goto))

(defun t3php-toc-goto-line-and-hide ()
  "Go to the location and hide the *t3php-toc* window."
  (interactive)
  (t3php-toc-visit-location 'hide))

(defun t3php-toc-toggle-follow ()
  "Toggle follow mode in *t3php-toc* buffer."
  (interactive)
  (setq t3php-toc-last-follow-point -1)
  (setq t3php-toc-follow-mode (not t3php-toc-follow-mode)))

(defun t3php-toc-rescan ()
  "Regenerate *t3php-toc* buffer by reparsing the t3php buffer."
  (interactive)
  (switch-to-buffer-other-window (get-file-buffer t3php-toc-last-file))
  (t3php-toc t))

(defun t3php-toc-quit-and-kill ()
  "Kill the *t3php-toc* buffer."
  (interactive)
  (kill-buffer "*t3php-toc*")
  (unless (one-window-p)
    (delete-window))
  (switch-to-buffer (marker-buffer t3php-toc-return-marker))
  (t3php-re-enlarge)
  (goto-char (marker-position t3php-toc-return-marker)))

(defun t3php-toc-beginning-of-buffer ()
  "Go to the beginning of the *t3php-toc* buffer."
  (interactive)
  (goto-line 4)
  (recenter))

(defun t3php-toc-end-of-buffer ()
  "Go to the end of the *t3php-toc* buffer."
  (interactive)
  (goto-line (line-number-at-pos (point-max))))

(defun t3php-toc-content (t3php-buffer)
  "Returns table of contents of methods

This is a list of lists containing the method name, method start and end position."
  (save-current-buffer
    ;; Processing a buffer with lots of markers slows processing, like inserting
    ;; or deleting text, in a buffer. Unreferenced markers are garbage collected
    ;; eventually, but until then will continue to use time if they do point
    ;; somewehere. Therefore it is helpfull to make a marker point nowhere, when
    ;; it is not used anymore. Thus clear all markers in the list
    ;; `t3php-toc-marker-list', which keeps track of all markers.
    (when t3php-toc-marker-list
      (dolist (t3php-toc-marker t3php-toc-marker-list)
	(set-marker t3php-toc-marker nil))
      (setq t3php-toc-marker-list (list)))

    ;; Switch to t3php buffer.
    (set-buffer t3php-buffer)

    (let ((start (point-min))
	  (end (point-max))
	  (block-end (point-min))
	  tbs
	  list-of-blocks)
      (save-excursion
	(goto-char start)

	(catch 'no-valid-block
	  (while t
	    (let ((block-marker (make-marker))
		  (block-start
		   ;; Look for method block start
		   (save-excursion
		     (goto-char block-end)
		     (setq tbs (re-search-forward
				"^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function.*" nil t))
		     ;; If search was successfull set block-start to the beginning
		     ;; of the line; return nil otherwise
		     (if (not tbs)
			 nil
		       (goto-char tbs)
		       (beginning-of-line)
		       (point)))))
	      (if block-start
		  (let ((block-name
			 ;; Save method name
			 (save-excursion
			   (goto-char block-start)
			   (looking-at
			    (concat
			     "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function\\s-+"
			     "\\(\\w+?\\)\\s-*("))
			   (match-string 1))))
		    ;; Look for measurment block end
		    (save-excursion
		      (goto-char block-start)
		      (setq block-end (re-search-forward "}" nil t)))
		    ;; The following local variables are defined up to here:
		    ;; [1] block-start: point of measurement block start, at the beginning
		    ;;                  of the line; nil otherwise
		    ;; [2] block-name : name of measurement block
		    ;; [3] block-end  : point of measurment block end, at the end of the
		    ;;                  `end measurement' string; nil otherwise
		    (if (and (<= block-start end)
			     (<= block-end end))
			(progn
			  (set-marker block-marker block-start)
			  (push block-marker t3php-toc-marker-list)
			  (push (list
				 (line-number-at-pos block-start)  ; block start
				 (line-number-at-pos block-end)    ; block end
				 block-name                        ; block name
				 block-marker)                     ; block marker
				list-of-blocks))
		      (throw 'no-valid-block t)))
		(throw 'no-valid-block t))))))
      (reverse list-of-blocks))))

(defun t3php-toc-format (content)
  "Return a list of formatted lines for the table of contents.
CONTENT is the list of lists returned by function `t3php-toc-content'."
  (let* ((formatting-information (t3php-toc-formatting-information content))
	 (formatted-content (list))
	 (max-block-name-length (nth 0 formatting-information))
	 (max-block-inf-from-length (nth 1 formatting-information))
	 (max-block-inf-to-length (nth 2 formatting-information))
	 (max-block-inf-length (nth 3 formatting-information))
	 block-start
	 block-end
	 block-name
	 block-marker
	 (formatter (concat "* %-"
			    (number-to-string (+ max-block-name-length 6))
			    "s"
			    "[ %"
			    (number-to-string max-block-inf-from-length)
			    "d-%-"
			    (number-to-string max-block-inf-to-length)
			    "d ]\n"))
	 formatted-line)
    (dolist (line content)
      (setq block-start (nth 0 line))
      (setq block-end (nth 1 line))
      (setq block-name (nth 2 line))
      (setq block-marker (nth 3 line))

      (remove-text-properties 0 (length block-name)
			 '(face nil) block-name)
      (put-text-property 0 (length block-name)
			 'font-lock-face `(:foreground
					   ,t3php-toc-block-name-color)
			 block-name)
      (put-text-property 0 (length block-name)
			 'help-echo block-name
			 block-name)
      (put-text-property 0 (length block-name)
			 'fontified t
			 block-name)

      (setq formatted-line
	    (if (> (length block-name) max-block-name-length)
		(format formatter (concat (substring block-name
						     0
						     max-block-name-length)
					  "...")
			block-start block-end)
	      (format formatter block-name block-start block-end)))
      ;; The asterisk `*' holds the block information like `block-name' and
      ;; `block-marker' as a text property.
      (put-text-property 0 1 'data `(,block-marker ,block-name) formatted-line)

      (push formatted-line formatted-content))

    ;; Remove last newline character of last element in `formatted-content'.
    (unless (null formatted-content)
      (push (substring (pop formatted-content) 0 -1) formatted-content))

    (reverse formatted-content)))

(defun t3php-toc-formatting-information (content)
  "Return a list of formatting information for the table of contents.
The first item is the maximum string length allowed for measurement block
names.  The value is limited by the current window width with respect to the
length of a string, which shows the measurement block start and end lines
aligned to the right border of the window.  The string sizes of the latter values
are also evaluated and provide the second and third item of the information
list. The fourth item is made of their sum plus an aditional value. Thus we have
a list like this: (max-block-name-length max-block-inf-from-size
max-block-inf-to-size max-block-inf-size). CONTENT is the list of lists returned
by function `t3php-toc-content'"
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

(defun t3php-toc-visit-location (visit-mode)
  "Visit t3php buffer according to VISIT-MODE."
  (let* ((toc-data (get-text-property (point) 'data))
	 (toc-window (selected-window))
	 (t3php-marker (nth 0 toc-data))
	 (t3php-block-name (nth 1 toc-data))
	 show-window show-buffer
	 match)

    (unless toc-data (message "%s" "Don't know which toc line to visit."))

    ;; --- t3php TOC BUFFER ---
    (princ t3php-block-name)
    (setq match
	  (cond
	   ((and (markerp t3php-marker) (marker-buffer t3php-marker))
	    ;; Marker is available and buffer still exists.
	    (switch-to-buffer-other-window (marker-buffer t3php-marker))
	    ;; --- t3php TOC BUFFER -> t3php BUFFER ---
	    (goto-char (marker-position t3php-marker))
	    (looking-at (concat
			 "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(?:\\(?:private\\|protected\\|public\\)\\s-+\\)?\\(?:static\\s-+\\)?function\\s-+"
			 t3php-block-name)))
	   (t
	    ;; Marker is lost. A backup method might be implemented in the
	    ;; future. For now just print an error message.
	    (message "Marker is lost."))))

    (when match
      (goto-char (match-beginning 0))
      (if (not (= (point) (point-max)))
	  (recenter 1))
      (t3php-highlight 1 (match-beginning 0) (match-end 0) (current-buffer))
      (add-hook 'pre-command-hook 't3php-highlight-shall-vanish))

    (setq show-window (selected-window)
	  show-buffer (current-buffer))

    (if (not match)
	(progn
	  (select-window toc-window)
	  ;; --- t3php TOC BUFFER -> t3php BUFFER ---
	  (message "Cannot find location."))

      ;; Now VISIT-MODE decides what to do next.
      (cond
       ((eq visit-mode 'view)
	(select-window toc-window))
       ((eq visit-mode 'goto)
	(t3php-unhighlight 1)
	(select-window show-window))
       ((eq visit-mode 'hide)
	(select-window toc-window)
	(t3php-unhighlight 1)
	(unless (one-window-p)
	  (delete-window))
	(if (window-live-p show-window)
	    (set-buffer show-buffer)
	  (switch-to-buffer show-buffer))
	(t3php-re-enlarge)
	;; --- t3php TOC BUFFER -> t3php BUFFER ---
	)
       (t nil)))))

(provide 't3php-mode)

;;; t3php-mode.el ends here
