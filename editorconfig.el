;;; editorconfig.el --- EditorConfig Emacs extension

;; Copyright (C) 2011-2012 EditorConfig Team

;; Author: Trey Hunner
;; Version: 0.1
;; Status: disfunctional
;; Homepage: http://editorconfig.org

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; EditorConfig helps developers define and maintain consistent
;; coding styles between different editors and IDEs.

;; The EditorConfig project consists of a file format for defining
;; coding styles and a collection of text editor plugins that enable
;; editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with
;; version control systems.

;;; Code:

(defvar edconf-exec-path "editorconfig")

(defun edconf-set-indentation (style &optional size tabwidth)
  "Set indentation type from given STYLE, SIZE and TABWIDTH."
  (if (equal style "space")
      ;; TODO Don't just set all these variables
      ;;      ie just those relevant for (derived-mode-p ...)
      ;; XXX  what if size isn't set?
      ;; XXX  I would *think* these variables automatically
      ;;      become buffer local when set "in any fashion",
      ;;      but you have to make sure
      (setq indent-tabs-mode nil
	    size (string-to-number size)
	    c-basic-offset size
	    python-indent size
	    py-indent-offset size
	    perl-indent-level size
	    cperl-indent-level size
	    tab-stop-list (let ((stops (list size)))
			    (while (< (car stops) 120)
			      (push (+ size (car stops)) stops))
			    (nreverse stops)))
    (setq indent-tabs-mode t))
  (when tabwidth
    (setq tab-width (string-to-number tabwidth))))

;; XXX  I *think* there are special variables for that.
;;      Also Emacs itself should detect what is actually used in
;;      the file and then *convert* to what we want if necessary.
(defun edconf-set-line-ending (end-of-line)
  "Set line ending style to CR, LF, or CRLF." ; all-caps here is for variables
  (set-buffer-file-coding-system
   (cond ((equal end-of-line "lf") 'undecided-unix)
	 ((equal end-of-line "cr") 'undecided-mac)
	 ((equal end-of-line "crlf") 'undecided-dos))))

;; XXX  Is the file relevant or would the directory do?
;;      If the latter is the case drop the argument and use
;;      the special default-directory variable.
;; NOTE hash-table? seriously? this is lisp use lists. :-)
;;      Performance won't matter here (and hash-table
;;      actually performs worse for so few items).
(defun edconf-get-properties (file)
  "Call EditorConfig core and return output as alist."
  (with-temp-buffer
    ;; XXX  How does this tell me that there is actually an
    ;;      editorconfig config file? (so that I can stop if
    ;;      there is not)?
    (call-process edconf-exec-path nil t nil file)
    (goto-char (point-min))
    (let (props)
      (while (re-search-forward "^\\([^ ]+\\) *= *\\([^ ]+\\)$")
	;; XXX  does editorconfig filter out garbage?
	(push (cons (match-string 1)
		    (match-string 2)) props))
      ;; XXX  what about duplicates? since this is an alist with
      ;;      new elements pushed to the front, later settings
      ;;      win here.
      props)))

;; NOTE Don't use let, set variables using setq and then only
;;      use once; it makes the code a bit longer than needed.
(defun edconf-setup ()
  (let ((props (edconf-get-properties
		(buffer-file-name (current-buffer)))))
    (edconf-set-indentation (cdr (assoc "indent_style" props))
			    (cdr (assoc "indent_size"  props))
			    (cdr (assoc "tab_width"    props)))
    (edconf-set-line-ending (cdr (assoc "end_of_line"  props)))))

;; TODO Control using a minor mode which activates only if a
;;      editorconfig config file is found up the directory tree.
(add-hook 'find-file-hook 'edconf-setup)

(provide 'editorconfig)
;;; editorconfig.el ends here
