;;; csv-align.el --- A collection of usefull functions

;; Copyright (C) 2009  Joachim Mathes
;;
;; Author: Joachim Mathes <joachim <underscore> mathes <at> web <dot> de>
;; Created: November 2009
;; Version: 0.1.0 $Id$
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

;; This is a collection of useful functions.
;; Every function is prefixed with `joma' which refers to my name and is used
;; as a namespace.

;; This file is *NOT* part of GNU Emacs.

;;; Code:
(defun joma-csv-align (separator)
  "Align columns of a buffer visting a csv file.
The SEPARATOR parameter specifies the column separator.  The default value is a
comma `,'"
  (interactive "cCSV Separator:")
  (princ "Operating...")
  (let ((formatter (list))
        (line-elements-lengths (list)))
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (let ((element-position 0)
              (line-elements-lengths-temp (list)))
          (beginning-of-line)
          (setq line-elements (split-string
                               (buffer-substring (line-beginning-position) (line-end-position))
                               (char-to-string separator)))
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

    (dolist (padding line-elements-lengths)
      (push (concat "%-"
                    (number-to-string padding)
                    "s")
            formatter))
    (setq formatter (reverse formatter))
    (princ formatter)

    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (let ((element-position 0))
        (beginning-of-line)
    	(setq line-elements (split-string
    			     (delete-and-extract-region (line-beginning-position) (line-end-position))
    			     (char-to-string separator)))
    	(dolist (element line-elements)
    	  (insert (format (nth element-position formatter) (nth element-position line-elements)))
	  (if (< element-position (1- (length line-elements)))
	      (insert (char-to-string separator)))
    	  (setq element-position (1+ element-position)))
        (forward-line)))))
  (princ "Operating...done"))

(provide 'csv-align)

;;; csv-align.el ends here
