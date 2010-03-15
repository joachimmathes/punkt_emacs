;;; csv-align.el --- A major mode for editing CSV files

;; Copyright (C) 2009  Joachim Mathes
;;
;; Author: Joachim Mathes <joachim <underscore> mathes <at> web <dot> de>
;; Created: November 2009
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

;;; Commentary

;; This is a major mode for editing CSV files.

;; Installation:

;; To install just drop this file into a directory on your load-path and
;; byte-compile it.  To set up Emacs to automatically edit files ending in ".ts"
;; using ts-mode add the following to your ~/.emacs file (GNU Emacs) or
;; ~/.xemacs/init.el file (XEmacs):
;;    (setq auto-mode-alist (cons '("\\.csv$" . csv-mode) auto-mode-alist))
;;    (autoload 'csv-mode "csv-mode" "CSV file editing mode." t)

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(defconst csv-version "0.0.1"
  "`csv-mode' version number.")

;;;; User definable variables

(defgroup csv nil
  "Major mode for editing CSV files."
  :prefix "csv-"
  :group 'data)

(defcustom csv-separator ","
  "Default csv separator."
  :type 'string
  :group 'csv)

(defcustom csv-field-color "DarkSlateBlue"
  "The color used to highlight a field.
The default value is `DarkSlateBlue'. For a list of all available colors use `M-x
list-colors-display"
  :type 'color
  :group 'csv)

;;;; Constants

(defconst csv-font-lock-keywords
  (list
   (cons csv-separator font-lock-comment-face))
   "Low level highlighting for PHP mode.")

;;;; Internal variables

(defvar csv-current-separator nil
  "Current separator.")

(defvar csv-mode-syntax-table nil
  "Syntax table used in CSV Mode buffers.")

(defvar csv-mode-map ()
  "Key map used in CSV Mode buffers.")

(defvar csv-mode-hook nil
  "Hook called by `csv-mode'.")

(defvar csv-highlight-overlays [nil nil]
  "A vector of different overlay to do highlighting.
This vector concerns only highlighting of horizontal lines.")

(defvar csv-field-position 1
  "Field position at point in line.")

(defvar csv-motion-mode 'field
  "Motion mode.")

;;;; Functions

;;;###autoload
(defun csv-mode ()
  "Major mode for editing csv files.
Bug reports, suggestions for new features and critics should go to
`joachim <underscore> mathes <at> web <dot> de'.

COMMANDS
\\{csv-mode-map}
VARIABLES"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'show-paren-mode)
  (make-local-variable 'csv-current-separator)

  (when (not csv-mode-syntax-table)
    (setq csv-mode-syntax-table (make-syntax-table)))
  (set-syntax-table csv-mode-syntax-table)

  (if csv-mode-map
      nil
    (setq csv-mode-map (make-sparse-keymap))
    (define-key csv-mode-map "\C-cf" 'csv-forward-column)
    (define-key csv-mode-map "\C-cb" 'csv-backward-column)
    (define-key csv-mode-map "\C-c\C-ar" 'csv-align-records-in-region)
    (define-key csv-mode-map "\C-c\C-ab" 'csv-align-records-in-buffer)
    (define-key csv-mode-map "\C-c\C-ss" 'csv-set-separator)
    (define-key csv-mode-map "\C-c\C-tm" 'csv-toggle-motion-mode)
    (define-key csv-mode-map "\C-c\C-f" 'csv-echo-column-number)
    (substitute-key-definition
     'forward-char 'csv-move-right csv-mode-map global-map)
    (substitute-key-definition
     'backward-char 'csv-move-left csv-mode-map global-map))
  (use-local-map csv-mode-map)

  (add-hook 'pre-command-hook
            'csv-pre-command-overlay-hook nil t)
  (add-hook 'post-command-hook
            'csv-post-command-overlay-hook nil t)

  ;; Initialize the overlays for highlighting fields.
  (dotimes (index (length csv-highlight-overlays))
    (when (not (aref csv-highlight-overlays index))
      (aset csv-highlight-overlays index (make-overlay 1 1))
      (overlay-put (aref csv-highlight-overlays index)
                   'category 'csv-field-overlay)
      (overlay-put (aref csv-highlight-overlays index)
                   'font-lock-face `(:background ,csv-field-color))))

  (setq major-mode 'csv-mode
        mode-name "CSV"
	csv-current-separator csv-separator
        font-lock-defaults '(csv-font-lock-keywords)
        show-paren-mode t
	cursor-type nil)

  (if csv-mode-hook
      (run-hooks 'csv-mode-hook)))

(defun csv-set-separator (separator)
  "Set separator."
  (interactive "cSeparator: ")
  (setq csv-current-separator (string separator)
	font-lock-keywords (list `(,(symbol-value 'csv-current-separator) . font-lock-comment-face)))
  (font-lock-default-fontify-buffer))

(defun csv-align-records-in-buffer ()
  "Align columns of a buffer visting a csv file."
  (interactive)
  (csv-align-records-in-region (point-min) (point-max)))

(defun csv-align-records-in-region (start end)
  "Align columns in a region of a buffer visting a csv file."
  (interactive "r")
  (let (block-start block-end)
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (setq block-start (point))
      (goto-char end)
      (end-of-line)
      (setq block-end (point)))
    (csv-align-records block-start block-end)))

(defun csv-align-records (start end)
  "Align columns of a buffer visting a csv file."
  (message "Operating...")

  ;; Analyze csv lines
  (let ((formatter (list))
        (line-elements-lengths (list))
	(last-line (line-number-at-pos end)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((element-position 0)
              (line-elements-lengths-temp (list)))
          (beginning-of-line)
          (setq line-elements (split-string
                               (buffer-substring (line-beginning-position) (line-end-position))
                               csv-current-separator))
          (dolist (element line-elements)
            (cond ((eq nil (nth 0 line-elements-lengths))
                   (push (length element) line-elements-lengths-temp))
                  ((>  (length element) (nth 0 line-elements-lengths))
                   (push (length element) line-elements-lengths-temp))
                  ((<=  (length element) (nth 0 line-elements-lengths))
                   (push (nth 0 line-elements-lengths) line-elements-lengths-temp)))
            (pop line-elements-lengths)
            (setq element-position (1+ element-position)))
          (if (> (length line-elements-lengths) 0)
              (setq line-elements-lengths (nconc (nreverse line-elements-lengths-temp) line-elements-lengths))
            (setq line-elements-lengths (nreverse line-elements-lengths-temp)))
          (forward-line))))

    ;; Build line formatter
    (dolist (padding line-elements-lengths)
      (push (concat "%-"
                    (number-to-string padding)
                    "s")
            formatter))
    (setq formatter (reverse formatter))

    ;; Align csv lines
    (save-excursion
      (goto-char start)
      (while (<= (line-number-at-pos (point)) last-line)
        (let ((element-position 0))
        (beginning-of-line)
    	(setq line-elements (split-string
    			     (delete-and-extract-region (line-beginning-position) (line-end-position))
    			     csv-current-separator))
    	(dolist (element line-elements)
    	  (insert (format (nth element-position formatter) (nth element-position line-elements)))
	  (if (< element-position (1- (length line-elements)))
	      (insert csv-current-separator))
    	  (setq element-position (1+ element-position)))
        (forward-line)))))
  (message "done"))

(defun csv-forward-column (arg)
  "Move point forward ARG columns."
  (interactive "p")
  (re-search-forward csv-current-separator nil 'limit arg))

(defun csv-backward-column (arg)
  "Move point backward ARG columns."
  (interactive "p")
  (re-search-backward csv-current-separator nil t)
  (if (re-search-backward csv-current-separator nil 'limit arg)
      (forward-char)))

(defun csv-pre-command-overlay-hook ()
  "Deactivates the overlay of the current field.
Used in `pre-command-hook'."
  (csv-unhighlight 0))

(defun csv-post-command-overlay-hook ()
  "Activates the field overlay of the current line.
Used in `post-command-hook'."
  (let (field-start field-end)
    (save-excursion
      (re-search-backward csv-current-separator (line-beginning-position) 'foo 1)
      (setq field-start (point))
      (if (not (eobp))
	  (forward-char))
      (re-search-forward csv-current-separator (line-end-position) 'foo 1)
      (setq field-end (point)))
    (csv-highlight 0
		   field-start
		   field-end)))

(defun csv-highlight (index begin end)
  "Highlight a region with overlay INDEX.
The region is described by the delimiters BEGIN and END.
Highlighting is handled with overlays.  Different indices map to different
overlays."
  (move-overlay (aref csv-highlight-overlays index)
                begin end (current-buffer)))

(defun csv-unhighlight (index)
  "Detach overlay INDEX."
  (delete-overlay (aref csv-highlight-overlays index)))

(defun csv-toggle-motion-mode ()
  "Toggle motion mode."
  (interactive)
  (if (eq csv-motion-mode 'field)
      (progn
	(substitute-key-definition 'csv-move-right 'forward-char csv-mode-map)
	(substitute-key-definition 'csv-move-left 'backward-char csv-mode-map)
	(setq cursor-type t)
	(setq csv-motion-mode 'char))
    (substitute-key-definition 'forward-char 'csv-move-right csv-mode-map global-map)
    (substitute-key-definition 'backward-char 'csv-move-left csv-mode-map global-map)
    (setq cursor-type nil)
    (setq csv-motion-mode 'field)))

(defun csv-move-right ()
  "Move right in terms of fields"
  (interactive)
  (re-search-forward csv-current-separator (line-end-position) 'foo 2)
  (backward-char))

(defun csv-move-left ()
  "Move right in terms of fields"
  (interactive)
  (re-search-backward csv-current-separator (line-beginning-position) 'foo 1))

(defun csv-echo-column-number ()
  "Echo column number in echo area."
  (interactive)
  (message "Field %d" (csv-evaluate-column-number)))

(defun csv-evaluate-column-number ()
  "Evaluate column number."
  (let ((csv-number-of-columns 1))
    (save-excursion
      (if (not (eq (point) (line-beginning-position)))
	  (backward-char))
      (while (not (eq (point) (line-beginning-position)))
	(if (string= (char-to-string (char-after (point))) csv-current-separator)
	    (setq csv-number-of-columns (1+ csv-number-of-columns)))
	(backward-char)))
  csv-number-of-columns))

(provide 'csv-mode)

;;; csv-align.el ends here
