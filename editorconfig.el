;;; editorconfig.el --- EditorConfig Emacs extension

;; Copyright (C) 2011-2013 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.3
;; URL: http://github.com/editorconfig/editorconfig-emacs#readme

;; See
;; http://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

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
  (set (make-local-variable 'web-mode-indent-style) 2)
  (when (equal style "space")
    (set (make-local-variable 'indent-tabs-mode) nil)
    (when size
      (setq size (string-to-number size))
      (set (make-local-variable 'LaTeX-indent-level) size)
      (set (make-local-variable 'LaTeX-item-indent) size)
      (set (make-local-variable 'TeX-brace-indent-level) size)
      (set (make-local-variable 'c-basic-offset) size)
      (set (make-local-variable 'cmake-tab-width) size)
      (set (make-local-variable 'coffee-tab-width) size)
      (set (make-local-variable 'cperl-indent-level) size)
      (set (make-local-variable 'css-indent-offset) size)
      (set (make-local-variable 'haskell-indent-offset) size)
      (set (make-local-variable 'shm-indent-spaces) size)
      (set (make-local-variable 'js-indent-level) size)
      (set (make-local-variable 'js2-basic-offset) size)
      (set (make-local-variable 'js3-indent-level) size)
      (set (make-local-variable 'lisp-indent-offset) size)
      (set (make-local-variable 'perl-indent-level) size)
      (set (make-local-variable 'py-indent-offset) size)
      (set (make-local-variable 'python-indent) size)
      (set (make-local-variable 'ruby-indent-level) size)
      (set (make-local-variable 'sh-basic-offset) size)
      (set (make-local-variable 'sh-indentation) size)
      (set (make-local-variable 'web-mode-markup-indent-offset) size)
      (set (make-local-variable 'web-mode-css-indent-offset) size)
      (set (make-local-variable 'web-mode-code-indent-offset) size)
      ;(set (make-local-variable 'sgml-basic-offset) size)
      (set (make-local-variable 'tab-stop-list) (let ((stops (cons size ())))
                                                  (while (< (car stops) 120)
                                                    (setq stops (cons
                                                                 (+ size (car stops))
                                                                 stops)))
                                                  (nreverse stops)))))
  (when (equal style "tab")
    (set (make-local-variable 'indent-tabs-mode) t))
  (if tab_width
      (set (make-local-variable 'tab-width) (string-to-number tab_width))))

(defun edconf-set-line-ending (end-of-line)
  "Set line ending style to CR, LF, or CRLF"
  (set-buffer-file-coding-system
   (cond
    ((equal end-of-line "lf") 'undecided-unix)
    ((equal end-of-line "cr") 'undecided-mac)
    ((equal end-of-line "crlf") 'undecided-dos)
    (t 'undecided))
   nil t))

(defun edconf-set-trailing-nl (final-newline)
  (cond
   ((equal final-newline "true")
    ;; keep prefs around how/when the nl is added, if set - otherwise add on save
    (set      (make-local-variable 'require-final-newline)      (or require-final-newline t))
    (set      (make-local-variable 'mode-require-final-newline) (or mode-require-final-newline t)))
   ((equal final-newline "false")
    ;; FIXME: Add functionality for actually REMOVING any trailing newlines here!
    ;;        (rather than just making sure we don't automagically ADD a new one)
    (set      (make-local-variable 'require-final-newline) nil)
    (set      (make-local-variable 'mode-require-final-newline) nil))))

(defun edconf-set-trailing-ws (trim-trailing-ws)
  "set up trimming of trailing whitespace at end of lines"
  (make-local-variable 'write-file-functions) ;; just current buffer
  (when (equal trim-trailing-ws "true")
    ;; when true we push delete-trailing-whitespace (emacs > 21)
    ;; to write-file-functions
    (add-to-list
     'write-file-functions
     'delete-trailing-whitespace))
  (when (equal trim-trailing-ws "false")
    ;; when false we remove every delete-trailing-whitespace
    ;; from write-file-functions
    (setq
     write-file-functions
     (delete
      'delete-trailing-whitespace
      write-file-functions))))

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
        (when (> (length key-val) 1)
          (let ((key (intern (car key-val)))
                (val (mapconcat 'identity (cdr key-val) "")))
            (puthash key val properties)))))))

;;;###autoload
(defun edconf-find-file-hook ()
  (when (executable-find edconf-exec-path)
    (let ((props (edconf-parse-properties (edconf-get-properties))))
      (edconf-set-indentation (gethash 'indent_style props)
                              (gethash 'indent_size props)
                              (gethash 'tab_width props))
      (edconf-set-line-ending (gethash 'end_of_line props))
      (edconf-set-trailing-nl (gethash 'insert_final_newline props))
      (edconf-set-trailing-ws (gethash 'trim_trailing_whitespace props)))))

;;;###autoload
(add-hook 'find-file-hook 'edconf-find-file-hook)

(provide 'editorconfig)

;;; editorconfig.el ends here
