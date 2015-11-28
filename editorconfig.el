;;; editorconfig.el --- EditorConfig Emacs Plugin

;; Copyright (C) 2011-2015 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.5
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((editorconfig-core "20151107.831"))

;; See
;; https://github.com/editorconfig/editorconfig-emacs/graphs/contributors
;; or the CONTRIBUTORS file for the list of contributors.

;; This file is part of EditorConfig Emacs Plugin.

;; EditorConfig Emacs Plugin is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; EditorConfig Emacs Plugin is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; EditorConfig Emacs Plugin. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; EditorConfig helps developers define and maintain consistent
;; coding styles between different editors and IDEs.

;; The EditorConfig project consists of a file format for defining
;; coding styles and a collection of text editor plugins that enable
;; editors to read the file format and adhere to defined styles.
;; EditorConfig files are easily readable and they work nicely with
;; version control systems.

;;; Code:

(defcustom editorconfig-exec-path
  "editorconfig"
  "EditorConfig command"
  :type 'string
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-exec-path
  'editorconfig-exec-path
  "0.5")

(defcustom editorconfig-get-properties-function
  'editorconfig-get-properties-from-exec
  "Function to get EditorConofig properties for current buffer.
This function will be called with no argument and should return a hash object
containing properties, or nil if any core program is not available.
The hash object should have symbols of property names as keys and strings of
property values as values."
  :type 'function
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-get-properties-function
  'editorconfig-get-properties-function
  "0.5")

(defcustom editorconfig-custom-hooks ()
  "A list of custom hooks after loading common EditorConfig settings

Each element in this list is a hook function. This hook function takes one
parameter, which is a property hash table. The value of properties can be
obtained through gethash function.

The hook does not have to be coding style related; you can add whatever
functionality you want. For example, the following is an example to add a new
property emacs_linum to decide whether to show line numbers on the left

(add-to-list 'editorconfig-custom-hooks
  '(lambda (props)
     (let ((show-line-num (gethash 'emacs_linum props)))
       (cond ((equal show-line-num \"true\") (linum-mode 1))
         ((equal show-line-num \"false\") (linum-mode 0))))))

"
  :type '(lambda (properties) (body))
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-custom-hooks
  'editorconfig-custom-hooks
  "0.5")

(defcustom editorconfig-indentation-alist
  '((awk-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (css-mode css-indent-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (erlang-mode erlang-indent-level)
    (groovy-mode c-basic-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  shm-indent-spaces)
    (idl-mode c-basic-offset)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js2-mode js2-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (latex-mode . editorconfig-set-indentation/latex-mode)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (mustache-mode mustache-basic-offset)
    (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
    (objc-mode c-basic-offset)
    (perl-mode perl-indent-level)
    (pike-mode c-basic-offset)
    (puppet-mode puppet-indent-level)
    (python-mode . editorconfig-set-indentation/python-mode)
    (ruby-mode ruby-indent-level)
    (scala-mode scala-indent:step)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset sh-indentation)
    (web-mode (web-mode-indent-style . (lambda (size) 2))
              web-mode-markup-indent-offset
              web-mode-css-indent-offset
              web-mode-code-indent-offset
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset))
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
(define-obsolete-variable-alias
  'edconf-indentation-alist
  'editorconfig-indentation-alist
  "0.5")

(defun editorconfig-string-integer-p (string)
  "Whether a string representing integer"
  (if (stringp string)
    (string-match-p "\\`[0-9]+\\'" string)
    nil))

(defun editorconfig-set-indentation/python-mode (size)
  (set (make-local-variable (if (or (> emacs-major-version 24)
                                    (and (= emacs-major-version 24)
                                         (>= emacs-minor-version 3)))
                                'python-indent-offset
                              'python-indent))
       size)
  ;; For https://launchpad.net/python-mode
  (when (boundp 'py-indent-offset)
    (set (make-local-variable 'py-indent-offset) size)))

(defun editorconfig-set-indentation/latex-mode (size)
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

(defun editorconfig-set-indentation (style &optional size tab_width)
  "Set indentation type from given style and size"
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'tab-width)
  (if (editorconfig-string-integer-p size)
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
    (when (featurep 'evil)
      (setq-local evil-shift-width size))
    (let ((parent major-mode)
          entry)
      ;; Find the closet parent mode of `major-mode' in
      ;; `editorconfig-indentation-alist'.
      (while (and (not (setq entry (assoc parent editorconfig-indentation-alist)))
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

(defun editorconfig-set-line-ending (end-of-line)
  "Set line ending style to CR, LF, or CRLF"
  (set-buffer-file-coding-system
   (cond
    ((equal end-of-line "lf") 'undecided-unix)
    ((equal end-of-line "cr") 'undecided-mac)
    ((equal end-of-line "crlf") 'undecided-dos)
    (t 'undecided))
   nil t))

(defun editorconfig-set-trailing-nl (final-newline)
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

(defun editorconfig-set-trailing-ws (trim-trailing-ws)
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

(defun editorconfig-set-line-length (length)
  "set the max line length (fill-column)"
  (when (editorconfig-string-integer-p length)
    (set-fill-column (string-to-number length))))

(defun editorconfig-get-properties ()
  "Call EditorConfig core and return output"
  (let ((oldbuf (current-buffer)))
    (call-process editorconfig-exec-path nil "ecbuffer" nil (buffer-file-name oldbuf))
    (set-buffer (get-buffer "ecbuffer"))
    (let (props-string)
      (setq props-string (buffer-string))
      (set-buffer oldbuf)
      (kill-buffer (get-buffer "ecbuffer"))
      props-string)))

(defun editorconfig-parse-properties (props-string)
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

(defun editorconfig-get-properties-from-exec ()
  "Get EditorConfig properties of current buffer by calling `editorconfig-exec-path'."
  (if (executable-find editorconfig-exec-path)
    (editorconfig-parse-properties (editorconfig-get-properties))
    (display-warning :error
      "Unable to find editorconfig executable.")
    nil))

;;;###autoload
(defun editorconfig-apply ()
  (when buffer-file-name
    (let ((props (and (functionp editorconfig-get-properties-function)
                   (funcall editorconfig-get-properties-function))))
      (if props
        (progn
          (editorconfig-set-indentation (gethash 'indent_style props)
            (gethash 'indent_size props)
            (gethash 'tab_width props))
          (editorconfig-set-line-ending (gethash 'end_of_line props))
          (editorconfig-set-trailing-nl (gethash 'insert_final_newline props))
          (editorconfig-set-trailing-ws (gethash 'trim_trailing_whitespace props))
          (editorconfig-set-line-length (gethash 'max_line_length props))
          (dolist (hook editorconfig-custom-hooks)

            (funcall hook props)))
        (display-warning :error "EditorConfig core program is not available.  Styles will not be applied.")))))

;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature."
  :global t
  :lighter ""
  (dolist (hook (list
                  'find-file-hook
                  'after-change-major-mode-hook))
    (if editorconfig-mode
      (add-hook hook 'editorconfig-apply)
      (remove-hook hook 'editorconfig-apply))))

;;;###autoload
(add-to-list 'auto-mode-alist '("/\\.editorconfig\\'" . conf-unix-mode))

(provide 'editorconfig)

;;; editorconfig.el ends here
