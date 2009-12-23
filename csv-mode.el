;;; csv-align.el --- A collection of usefull functions

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
  :group 'languages)

(defcustom csv-separator "|"
  "Default csv separator."
  :type 'string
  :group 'csv)

;;;; Constants

(defconst csv-font-lock-keywords
  (list
   (cons "|" font-lock-comment-face)
   "Low level highlighting for PHP mode."))

;;;; Internal variables

(defvar csv-mode-syntax-table nil
  "Syntax table used in CSV Mode buffers.")

(defvar csv-mode-map ()
  "Key map used in CSV Mode buffers.")

(defvar csv-mode-hook nil
  "Hook called by `csv-mode'.")

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

  (when (not csv-mode-syntax-table)
    (setq csv-mode-syntax-table (make-syntax-table)))
  (set-syntax-table csv-mode-syntax-table)

  (if csv-mode-map
      nil
    (setq csv-mode-map (make-sparse-keymap))
    (define-key csv-mode-map "\C-cf" 'csv-forward-column)
    (define-key csv-mode-map "\C-cb" 'csv-backward-column)
    (define-key csv-mode-map "\C-ca" 'csv-align-records))
  (use-local-map csv-mode-map)

  (setq major-mode 'csv-mode
        mode-name "CSV"
        font-lock-defaults '(csv-font-lock-keywords)
        show-paren-mode t)

  (if csv-mode-hook
      (run-hooks 'csv-mode-hook)))

(defun csv-align-records ()
  "Align columns of a buffer visting a csv file."
  (interactive)
  (message "Operating...")

  ;; Analyze csv lines
  (let ((formatter (list))
        (line-elements-lengths (list)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((element-position 0)
              (line-elements-lengths-temp (list)))
          (beginning-of-line)
          (setq line-elements (split-string
                               (buffer-substring (line-beginning-position) (line-end-position))
                               csv-separator))
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
      (goto-char (point-min))
      (while (not (eobp))
        (let ((element-position 0))
        (beginning-of-line)
    	(setq line-elements (split-string
    			     (delete-and-extract-region (line-beginning-position) (line-end-position))
    			     csv-separator))
    	(dolist (element line-elements)
    	  (insert (format (nth element-position formatter) (nth element-position line-elements)))
	  (if (< element-position (1- (length line-elements)))
	      (insert csv-separator))
    	  (setq element-position (1+ element-position)))
        (forward-line)))))
  (message "done"))

(defun csv-forward-column (arg)
  "Move point forward ARG columns."
  (interactive "p")
  (re-search-forward "|" nil 'limit arg))

(defun csv-backward-column (arg)
  "Move point backward ARG columns."
  (interactive "p")
  (re-search-backward "|" nil t)
  (if (re-search-backward "|" nil 'limit arg)
      (forward-char)))

(provide 'csv-mode)

;;; csv-align.el ends here
