;;; editorconfig.el --- EditorConfig Emacs Plugin

;; Copyright (C) 2011-2016 EditorConfig Team

;; Author: EditorConfig Team <editorconfig@googlegroups.com>
;; Version: 0.7.8
;; URL: https://github.com/editorconfig/editorconfig-emacs#readme
;; Package-Requires: ((cl-lib "0.5"))

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

(declare-function editorconfig-core-get-properties-hash
  "editorconfig-core"
  nil)

(defgroup editorconfig nil
  "EditorConfig Emacs Plugin.

EditorConfig Helps developers define and maintain consistent coding styles
between different editors and IDEs"
  :tag "EditorConfig"
  :prefix "editorconfig-"
  :group 'tools)

(defcustom editorconfig-exec-path
  "editorconfig"
  "EditorConfig executable name.

This executable is invoked by `editorconfig-call-editorconfig-exec'."
  :type 'string
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-exec-path
  'editorconfig-exec-path
  "0.5")

(defcustom editorconfig-get-properties-function
  'editorconfig-get-properties
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
  "A list of custom hooks after loading common EditorConfig settings.

Each element in this list is a hook function.  This hook function takes one
parameter, which is a property hash table.  The value of properties can be
obtained through gethash function.

The hook does not have to be coding style related; you can add whatever
functionality you want.  For example, the following is an example to add a new
property emacs_linum to decide whether to show line numbers on the left

  (add-hook 'editorconfig-custom-hooks
    '(lambda (props)
       (let ((show-line-num (gethash 'emacs_linum props)))
         (cond ((equal show-line-num \"true\") (linum-mode 1))
           ((equal show-line-num \"false\") (linum-mode 0))))))"
  :type 'hook
  :group 'editorconfig)
(define-obsolete-variable-alias
  'edconf-custom-hooks
  'editorconfig-custom-hooks
  "0.5")

(defcustom editorconfig-indentation-alist
  ;; For contributors: Sort modes in alphabetical order, please :)
  '((awk-mode c-basic-offset)
     (c++-mode c-basic-offset)
     (c-mode c-basic-offset)
     (cmake-mode cmake-tab-width)
     (coffee-mode coffee-tab-width)
     (cperl-mode cperl-indent-level)
     (crystal-mode crystal-indent-level)
     (css-mode css-indent-offset)
     (emacs-lisp-mode lisp-indent-offset)
     (erlang-mode erlang-indent-level)
     (ess-mode ess-indent-offset)
     (groovy-mode c-basic-offset)
     (haskell-mode haskell-indent-spaces
       haskell-indent-offset
       shm-indent-spaces)
     (idl-mode c-basic-offset)
     (jade-mode jade-tab-width)
     (java-mode c-basic-offset)
     (js-mode js-indent-level)
     (js-jsx-mode js-indent-level sgml-basic-offset)
     (js2-mode js2-basic-offset)
     (js2-jsx-mode js2-basic-offset sgml-basic-offset)
     (js3-mode js3-indent-level)
     (json-mode js-indent-level)
     (julia-mode julia-indent-offset)
     (latex-mode . editorconfig-set-indentation/latex-mode)
     (lisp-mode lisp-indent-offset)
     (livescript-mode livescript-tab-width)
     (lua-mode lua-indent-level)
     (matlab-mode matlab-indent-level)
     (mustache-mode mustache-basic-offset)
     (nginx-mode nginx-indent-level)
     (nxml-mode nxml-child-indent (nxml-attribute-indent . 2))
     (objc-mode c-basic-offset)
     (octave-mode octave-block-offset)
     (perl-mode perl-indent-level)
     (pike-mode c-basic-offset)
     (ps-mode ps-mode-tab)
     (puppet-mode puppet-indent-level)
     (python-mode . editorconfig-set-indentation/python-mode)
     (ruby-mode ruby-indent-level)
     (rust-mode rust-indent-offset)
     (scala-mode scala-indent:step)
     (scss-mode css-indent-offset)
     (sgml-mode sgml-basic-offset)
     (sh-mode sh-basic-offset sh-indentation)
     (slim-mode slim-indent-offset)
     (tcl-mode tcl-indent-level
       tcl-continued-indent-level)
     (typescript-mode typescript-indent-level)
     (web-mode (web-mode-indent-style . (lambda (size) 2))
       web-mode-markup-indent-offset
       web-mode-css-indent-offset
       web-mode-code-indent-offset
       web-mode-block-padding
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

(defcustom editorconfig-exclude-modes ()
  "List of major mode symbols not to apply properties."
  :type '(repeat (symbol :tag "Major Mode"))
  :group 'editorconfig)

(defvar editorconfig-properties-hash nil
  "Hash object of EditorConfig properties for current buffer.
Set by `editorconfig-apply' and nil if that is not invoked in current buffer
yet.")
(make-variable-buffer-local 'editorconfig-properties-hash)


(defun editorconfig-string-integer-p (string)
  "Return non-nil if STRING represents integer."
  (and (stringp string)
    (string-match-p "\\`[0-9]+\\'" string)))

(defun editorconfig-set-indentation/python-mode (size)
  "Set `python-mode' indent size to SIZE."
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
  "Set `latex-mode' indent size to SIZE."
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
  "Set indentation type from STYLE, SIZE and TAB_WIDTH."
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
      (set (make-local-variable 'evil-shift-width) size))
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

(defun editorconfig-set-coding-system (end-of-line charset)
  "Set buffer coding system by END-OF-LINE and CHARSET."
  (let ((eol (cond
               ((equal end-of-line "lf") 'undecided-unix)
               ((equal end-of-line "cr") 'undecided-mac)
               ((equal end-of-line "crlf") 'undecided-dos)
               (t 'undecided)))
         (cs (cond
               ((equal charset "latin1") 'iso-latin-1)
               ((equal charset "utf-8") 'utf-8)
               ((equal charset "utf-8-bom") 'utf-8-with-signature)
               ((equal charset "utf-16be") 'utf-16be)
               ((equal charset "utf-16le") 'utf-16le)
               (t 'undecided))))
    (unless (and (eq eol 'undecided)
              (eq cs 'undecided))
      (set-buffer-file-coding-system (merge-coding-systems
                                       cs
                                       eol)
        nil t))))

(defun editorconfig-set-trailing-nl (final-newline)
  "Set up requiring final newline by FINAL-NEWLINE."
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
  "Set up trimming of trailing whitespace at end of lines by TRIM-TRAILING-WS."
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
  "Set the max line length (fill-column) to LENGTH."
  (when (and (editorconfig-string-integer-p length)
          (> (string-to-number length) 0))
    (set-fill-column (string-to-number length))))

(defun editorconfig-call-editorconfig-exec ()
  "Call EditorConfig core and return output."
  (let ((filename (buffer-file-name)))
    (with-temp-buffer
      (setq default-directory "/")
      (if (eq 0
            (call-process editorconfig-exec-path nil t nil filename))
        (buffer-string)
        (error (buffer-string))))))

(defun editorconfig-parse-properties (props-string)
  "Create properties hash table from PROPS-STRING."
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
    (editorconfig-parse-properties (editorconfig-call-editorconfig-exec))
    (error "Unable to find editorconfig executable")))

(defun editorconfig-get-properties ()
  "Get EditorConfig properties of current buffer.

It calls `editorconfig-get-properties-from-exec' if
`editorconfig-exec-path` is found, otherwise
`editorconfig-core-get-properties-hash'."
  (if (executable-find editorconfig-exec-path)
    (editorconfig-get-properties-from-exec)
    (require 'editorconfig-core)
    (editorconfig-core-get-properties-hash)))

;;;###autoload
(defun editorconfig-display-current-properties ()
  "Display EditorConfig properties extracted for current buffer."
  (interactive)
  (if editorconfig-properties-hash
    (let (
           (buf (get-buffer-create "*EditorConfig Properties*"))
           (file buffer-file-name)
           (props editorconfig-properties-hash))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "# EditorConfig for %s\n" file))
        (maphash (lambda (k v)
                   (insert (format "%S = %s\n" k v)))
          props))
      (display-buffer buf))
    (message "Properties are not applied to current buffer yet.")
    nil))

;;;###autoload
(defun editorconfig-apply ()
  "Apply EditorConfig properties for current buffer.
This function ignores `editorconfig-exclude-modes' and always applies available
properties."
  (interactive)
  (when buffer-file-name
    (condition-case err
      (progn
        (unless (functionp editorconfig-get-properties-function)
          (error "Invalid editorconfig-get-properties-function value"))
        (let ((props (funcall editorconfig-get-properties-function)))
          (progn
            (setq editorconfig-properties-hash props)
            (editorconfig-set-indentation (gethash 'indent_style props)
              (gethash 'indent_size props)
              (gethash 'tab_width props))
            (editorconfig-set-coding-system
              (gethash 'end_of_line props)
              (gethash 'charset props))
            (editorconfig-set-trailing-nl (gethash 'insert_final_newline props))
            (editorconfig-set-trailing-ws (gethash 'trim_trailing_whitespace props))
            (editorconfig-set-line-length (gethash 'max_line_length props))
            (run-hook-with-args 'editorconfig-custom-hooks props))))
      (error
        (display-warning 'editorconfig
          (concat (error-message-string err)
            ".  Styles will not be applied.")
          :error)))))

(defun editorconfig-mode-apply ()
  "Apply EditorConfig properties for current buffer.
This function do the job only when the major mode is not listed in
`editorconfig-exclude-modes'."
  (when (and major-mode
          (not (memq major-mode
                 editorconfig-exclude-modes)))
    (editorconfig-apply)))


;;;###autoload
(define-minor-mode editorconfig-mode
  "Toggle EditorConfig feature.
When enabled EditorConfig properties will be applied to buffers when first
visiting files or changing major modes if the major mode is not listed in
`editorconfig-exclude-modes'."
  :global t
  :lighter ""
  (dolist (hook '(after-change-major-mode-hook))
    (if editorconfig-mode
      (add-hook hook 'editorconfig-mode-apply)
      (remove-hook hook 'editorconfig-mode-apply))))

(provide 'editorconfig)

;;; editorconfig.el ends here
