;;; editorconfig.el --- EditorConfig Emacs extension

;; Copyright (C) 2011-2012 EditorConfig Team

;; Author: Trey Hunner
;; Version: 0.1
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

(defun edconf-set-indentation (style &optional size tab_width)
  "Set indentation type from given style and size"
  (when (equal style "space")
      (setq indent-tabs-mode nil
	    size (string-to-number size)
	    c-basic-offset size
	    python-indent size
	    py-indent-offset size
	    perl-indent-level size
	    cperl-indent-level size
	    tab-stop-list (let ((stops (cons size ())))
			    (while (< (car stops) 120)
			      (setq stops (cons
					   (+ size (car stops))
					   stops)))
                            (nreverse stops))))
  (when (equal style "tab")
    (setq indent-tabs-mode t))
  (if tab_width
      (setq tab-width (string-to-number tab_width))))

(defun edconf-set-line-ending (end-of-line)
  "Set line ending style to CR, LF, or CRLF"
  (set-buffer-file-coding-system
   (cond
    ((equal end-of-line "lf") 'undecided-unix)
    ((equal end-of-line "cr") 'undecided-mac)
    ((equal end-of-line "crlf") 'undecided-dos))))

(defun edconf-get-properties ()
  "Call EditorConfig core and return output"
  (let ((oldbuf (current-buffer)))
    (call-process edconf-exec-path nil "ecbuffer" nil (buffer-file-name oldbuf))
    (set-buffer (get-buffer "ecbuffer"))
    (let (props-string)
      (setq props-string (buffer-string))
      (set-buffer oldbuf)
      (kill-buffer (get-buffer "ecbuffer"))
      props-string)))

(defun edconf-parse-properties (props-string)
  "Create properties hash table from string of properties"
  (let (props-list properties)
    (setq props-list (split-string props-string "\n")
	  properties (make-hash-table :test 'equal))
    (dolist (prop props-list properties)
      (let ((key-val (split-string prop " *= *")))
	(if (> (length key-val) 1)
	    (let (key val)
	      (setq key (car key-val)
		    val (mapconcat 'identity (cdr key-val) ""))
	      (puthash key val properties)))))))

(add-hook 'find-file-hook
	  (function (lambda ()
		      (let (props indent_style indent_size tab_width)
			(setq props (edconf-parse-properties (edconf-get-properties))
			      indent_style (gethash "indent_style" props)
			      indent_size (gethash "indent_size" props)
			      tab_width (gethash "tab_width" props)
			      end_of_line (gethash "end_of_line" props))
			(edconf-set-indentation indent_style indent_size tab_width)
			(edconf-set-line-ending end_of_line)))))

(provide 'editorconfig)
