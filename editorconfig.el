;;; editorconfig.el --- EditorConfig Emacs extension

;; Copyright (C) 2011-2014 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.4
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

(defcustom edconf-exec-path
  "editorconfig"
  "EditorConfig command"
  :type 'string
  :group 'editorconfig)

(defcustom edconf-indentation-alist
  '((emacs-lisp-mode lisp-indent-offset)
    (lisp-mode lisp-indent-offset)
    (c-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (objc-mode c-basic-offset)
    (java-mode c-basic-offset)
    (idl-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (awk-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (css-mode css-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  shm-indent-spaces)
    (js-mode js-indent-level)
    (json-mode js-indent-level)
    (js2-mode js2-basic-offset)
    (js3-mode js3-indent-level)
    (perl-mode perl-indent-level)
    (python-mode . edconf-set-indentation/python-mode)
    (ruby-mode ruby-indent-level)
    (sh-mode sh-basic-offset sh-indentation)
    (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
    (sgml-mode sgml-basic-offset)
    (livescript-mode livescript-tab-width)
    (mustache-mode mustache-basic-offset)
    (scala-mode scala-indent:step)
    (groovy-mode c-basic-offset)
    (latex-mode . edconf-set-indentation/latex-mode)
    (web-mode (web-mode-indent-style . (lambda (size) 2))
              web-mode-markup-indent-offset
              web-mode-css-indent-offset
              web-mode-code-indent-offset
              web-mode-script-padding
              web-mode-style-padding)
    (erlang-mode erlang-indent-level))
  "Alist of indentation setting methods by modes.

Each element looks like (MODE . FUNCTION) or (MODE . INDENT-SPEC-LIST).

If FUNCTION is provided, it will be called when setting the indentation.  The
indent size will be passed.

If INDENT-SPEC-LIST is provided, each element of it must have one of the
following forms:

 1. VARIABLE

    It means (VARIABLE . 1).

 2. (VARIABLE . SPEC)

    Setting VARIABLE according to the type of SPEC:

      - Integer

        The value is (* SPEC INDENT-SIZE);

      - Function

        The value is (funcall SPEC INDENT-SIZE);

      - Any other type.

        The value is SPEC.

NOTE: Only the **buffer local** value of VARIABLE will be set."
  :type '(alist :key-type symbol :value-type sexp)
  :risky t
  :group 'editorconfig)

(defun edconf-string-integer-p (string)
  "Whether a string representing integer"
  (if (stringp string)
    (string-match-p "\\`[0-9]+\\'" string)
    nil))

(defun edconf-set-indentation/python-mode (size)
  (set (make-local-variable (if (or (> emacs-major-version 24)
                                    (and (= emacs-major-version 24)
                                         (>= emacs-minor-version 3)))
                                'python-indent-offset
                              'python-indent))
       size)
  ;; For https://launchpad.net/python-mode
  (when (boundp 'py-indent-offset)
    (set (make-local-variable 'py-indent-offset) size)))

(defun edconf-set-indentation/latex-mode (size)
  (set (make-local-variable 'tex-indent-basic) size)
  (set (make-local-variable 'tex-indent-item) size)
  (set (make-local-variable 'tex-indent-arg) (* 2 size))
  ;; For AUCTeX
  (when (boundp 'TeX-brace-indent-level)
    (set (make-local-variable 'TeX-brace-indent-level) size))
  (when (boundp 'LaTeX-indent-level)
    (set (make-local-variable 'LaTeX-indent-level) size))
  (when (boundp 'LaTeX-item-indent)
    (set (make-local-variable 'LaTeX-item-indent) (- size))))

(defun edconf-set-indentation (style &optional size tab_width)
  "Set indentation type from given style and size"
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'tab-width)
  (if (edconf-string-integer-p size)
    (setq size (string-to-number size))
    (when (not (equal size "tab")) (setq size nil)))
  (setq tab-width (cond (tab_width (string-to-number tab_width))
                        ((numberp size) size)
                        (t tab-width)))
  (when (equal size "tab")
    (setq size tab-width))
  (cond ((equal style "space")
         (setq indent-tabs-mode nil))
        ((equal style "tab")
         (setq indent-tabs-mode t)))
  (when size
    (let ((parent major-mode)
          entry)
      ;; Find the closet parent mode of `major-mode' in
      ;; `edconf-indentation-alist'.
      (while (and (not (setq entry (assoc parent edconf-indentation-alist)))
                  (setq parent (get parent 'derived-mode-parent))))
      (when entry
        (let ((fn-or-list (cdr entry)))
          (cond ((functionp fn-or-list) (funcall fn-or-list size))
                ((listp fn-or-list)
                 (dolist (elem fn-or-list)
                   (cond ((symbolp elem) (set (make-local-variable elem) size))
                         ((consp elem)
                          (let ((spec (cdr elem)))
                            (set (make-local-variable (car elem))
                                 (cond ((functionp spec) (funcall spec size))
                                       ((integerp spec) (* spec size))
                                       (t spec))))))))))))))

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

(defun edconf-set-line-length (length)
  "set the max line length (fill-column)"
  (when (edconf-string-integer-p length)
    (set-fill-column (string-to-number length))))

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
      (edconf-set-trailing-ws (gethash 'trim_trailing_whitespace props))
      (edconf-set-line-length (gethash 'max_line_length props)))))

;;;###autoload
(add-hook 'find-file-hook 'edconf-find-file-hook)

(provide 'editorconfig)

;;; editorconfig.el ends here
